#!/usr/bin/env python3
"""Check every abstract, author and generated page reference in the rendered PDF.

Requires Python 3, qpdf and pdftotext (Poppler). Run from the repository root.
"""
import csv
import json
import re
import subprocess
import sys
import unicodedata
import xml.etree.ElementTree as ET
from collections import Counter

pdf = sys.argv[1] if len(sys.argv) > 1 else 'uros2026_book_of_abstracts.pdf'
rows = list(csv.DictReader(open('abstracts.csv', encoding='utf-8')))
metadata = json.loads(subprocess.check_output(['qpdf', '--json', pdf]))
objects = metadata['qpdf'][1]

def obj(ref):
    return objects['obj:' + ref]['value']

catalog = next(v['value'] for v in objects.values()
               if isinstance(v.get('value'), dict) and v['value'].get('/Type') == '/Catalog')
destinations = obj(catalog['/Dests'])
page_numbers = {p['object']: i for i, p in enumerate(metadata['pages'], 1)}
xml = ET.fromstring(subprocess.check_output(['pdftotext', '-bbox', pdf, '-']))
ns = {'h': 'http://www.w3.org/1999/xhtml'}
pages = xml.findall('.//h:page', ns)
words = [p.findall('.//h:word', ns) for p in pages]

def normalize(value):
    return ''.join(c for c in unicodedata.normalize('NFKC', value).casefold() if c.isalnum())

def page_text(index):
    # Page-number footers are outside the content area.
    return ' '.join(w.text or '' for w in words[index] if float(w.attrib['yMin']) < 795)

texts = [page_text(i) for i in range(len(pages))]
index_start = page_numbers[destinations['/author-index'][0]]
practical_start = page_numbers[destinations['/practical-information'][0]]
body = normalize(' '.join(texts[:index_start - 1]))
external_links = {v['value'].get('/A', {}).get('/URI', '')[2:] for v in objects.values()
                  if isinstance(v.get('value'), dict) and v['value'].get('/Subtype') == '/Link'}
errors = []
expected_links = Counter()
for row in rows:
    dest = '/abstract-' + row['DOCID']
    if dest not in destinations:
        errors.append('Missing abstract destination: ' + row['TITLE'])
        continue
    page = page_numbers[destinations[dest][0]]
    if normalize(row['TITLE']) not in normalize(texts[page - 1]):
        errors.append('Title absent from destination page: ' + row['TITLE'])
    for field in ('ABSTRACT', 'LABOS'):
        source_text = row[field]
        for label, url in re.findall(r'\[([^\]]+)\]\(([^)]+)\)', source_text):
            if url not in external_links:
                errors.append('Missing abstract hyperlink: ' + url)
        source_text = re.sub(r'\[([^\]]+)\]\([^)]+\)', r'\1', source_text)
        if normalize(source_text) not in body:
            errors.append('Incomplete PDF text for ' + field + ': ' + row['TITLE'])
    author_keys = set()
    for author in row['Authors'].split(','):
        name = re.sub(r'\([0-9]+\)', '', author).strip()
        if normalize(name) not in normalize(texts[page - 1]):
            errors.append('Author missing at abstract: ' + name)
        author_keys.add(normalize(name))
    expected_links[dest] = len(author_keys)

actual_links = Counter()
checked_refs = 0
for i, page in enumerate(metadata['pages']):
    height = float(pages[i].attrib['height'])
    for ref in obj(page['object']).get('/Annots', []):
        link = obj(ref)
        dest = link.get('/Dest')
        if not dest:
            continue
        target_page = page_numbers[destinations[dest][0]]
        x1, y1, x2, y2 = link['/Rect']
        linked_words = []
        for word in words[i]:
            a = word.attrib
            x = (float(a['xMin']) + float(a['xMax'])) / 2
            y = (float(a['yMin']) + float(a['yMax'])) / 2
            if x1 - 1 <= x <= x2 + 1 and height - y2 - 1 <= y <= height - y1 + 1:
                linked_words.append(word.text or '')
        numbers = re.findall(r'\b\d+\b', ' '.join(linked_words))
        if str(target_page) not in numbers:
            errors.append(f'Incorrect printed page reference on page {i + 1}: {dest}, expected {target_page}, got {linked_words}')
        checked_refs += 1
        if index_start <= i + 1 < practical_start:
            actual_links[dest] += 1
if expected_links != actual_links:
    errors.append(f'Index coverage differs: missing {expected_links - actual_links}; extra {actual_links - expected_links}')
if 'Publication information' not in texts[1]:
    errors.append('Publication box is not on the reverse of the title page')
if 'Practical Conference Information' not in texts[-1]:
    errors.append('Practical information is not the final section')
for i, text in enumerate(texts):
    if not text.strip():
        errors.append(f'Unexpected blank page: {i + 1}')
if errors:
    print('\n'.join(errors))
    sys.exit(1)
print(f'Validated {len(rows)} complete abstracts, all author links, and {checked_refs} printed page references across {len(pages)} pages.')

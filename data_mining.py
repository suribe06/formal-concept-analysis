import os
import re
from PyPDF2 import PdfReader
from unidecode import unidecode
from nltk.tokenize import word_tokenize

with open('stopwords.txt', 'r') as file:
    stop_words = [line.rstrip() for line in file]

def extract_text_from_pdfs(merge=False):
    BASE_DIR = os.getcwd()
    ART_DIR = os.path.join(BASE_DIR, 'articles')
    R_DIR = os.path.join(BASE_DIR, 'R_Code', 'articles')
    text = ''
    for i, file in enumerate(os.listdir(ART_DIR)):
        # Read only PDF files
        if not file.endswith('.pdf'):
            continue
        article_path = os.path.join(ART_DIR, file)
        article_text = ""
        with open(article_path, 'rb') as f:
            pdf = PdfReader(f)
            for j, page in enumerate(pdf.pages):
                if j == 0: continue # Skip the first page
                # Extract the text from the page
                page_text = page.extract_text()
                # Convert text to ASCII
                page_text = unidecode(page_text)
                # Convert all symbols to lower case
                page_text = page_text.lower()
                # Remove numeric characters
                page_text = re.sub(r'\d+', '', page_text)
                # Remove non-alphanumeric characters
                page_text = re.sub(r"[^a-zA-Z]", " ", page_text)
                # Replace one or more whitespace characters with a single space
                page_text = re.sub(r'\s+', ' ', page_text)
                # Remove stop words
                tokens = word_tokenize(page_text)
                filtered_tokens = [token for token in tokens if token not in stop_words and len(token) > 2]
                article_text += ' '.join(filtered_tokens)
        if merge:
            text += article_text
        else:
            with open(os.path.join(R_DIR, f'article_{i}.txt'), 'w', encoding='utf-8') as text_file:
                text_file.write(article_text + '\n')
    if merge:
        with open(os.path.join(R_DIR, 'combined_articles.txt'), 'w', encoding='utf-8') as text_file:
            text_file.write(text + '\n')

extract_text_from_pdfs(merge=True)
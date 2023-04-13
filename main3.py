import os
import re
from PyPDF2 import PdfFileReader
from unidecode import unidecode
from nltk.tokenize import word_tokenize
from nltk.corpus import stopwords

stop_words = set(stopwords.words('english'))

def extract_text_from_pdfs(merge=False):
    BASE_DIR = os.getcwd()
    ART_DIR = os.path.join(BASE_DIR, 'articles')
    R_DIR = os.path.join(BASE_DIR, 'R_Code')

    text = ''
    for i, file in enumerate(os.listdir(ART_DIR)):
        # Read only PDF files
        if not file.endswith('.pdf'):
            continue
        article_path = os.path.join(ART_DIR, file)
        with open(article_path, 'rb') as f:
            pdf = PdfFileReader(f)
            article_text = ""
            for j, page in enumerate(pdf.pages):
                if j == 0: continue # Skip the first page
                # Extract the text from the page
                text = page.extract_text()
                # Convert text to ASCII
                text = unidecode(text)
                # Convert all symbols to lower case
                text = text.lower()
                # Remove numeric characters
                text = re.sub(r'\d+', '', text)
                # Remove non-alphanumeric characters
                text = re.sub(r"[^\w']+", " ", text)
                # Replace one or more whitespace characters with a single space
                text = re.sub(r'\s+', ' ', text)
                # Remove stop words
                tokens = word_tokenize(text)
                filtered_tokens = [token for token in tokens if token not in stop_words]
                article_text += ' '.join(filtered_tokens) + '\n'

        if merge:
            text += article_text
        else:
            with open(os.path.join(R_DIR, f'article_{i}.txt'), 'w', encoding='utf-8') as text_file:
                text_file.write(article_text)

    if merge:
        with open(os.path.join(R_DIR, 'combined_articles.txt'), 'w', encoding='utf-8') as text_file:
            text_file.write(text)

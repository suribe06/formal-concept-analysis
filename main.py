import os, re
from PyPDF2 import PdfReader
from unidecode import unidecode
from collections import defaultdict
from nltk.tokenize import word_tokenize
from nltk.corpus import stopwords

#stop_words = set(stopwords.words('spanish'))
stop_words = [unidecode(word) for word in set(stopwords.words('spanish'))]

BASE_DIR = os.getcwd()
ART_DIR = os.path.join(BASE_DIR, 'articles')

# Extracting the text of the PDF articles
articles_list = []
for file in [f for f in os.listdir(ART_DIR) if os.path.isfile(os.path.join(ART_DIR, f))]:
    article = os.path.join(ART_DIR, file)
    article_text = ''
    with open(article, 'rb') as file:
        # Create a PDF reader object
        reader = PdfReader(file)
        # Iterate through each page
        for page in reader.pages:
            # Extract the text from the page
            text = page.extract_text()
            # Remove numeric symbols
            text = re.sub(r'\d+', '', text)
            # Remove accents
            text = unidecode(text)
            # Remove non-letter symbols
            text = re.sub(r'[^a-zA-Z\d\']+', ' ', text)
            # Remove newline and tab characters
            text = text.replace('\n', ' ').replace('\t', ' ')
            # Remove whitespaces
            text = text.strip()
            # Convert all symbols to lower case
            text = text.lower()
            article_text += text
    articles_list.append(article_text)

for i, article_text in enumerate(articles_list):
    with open(f"article_{i}.txt", "w", encoding='utf-8') as text_file:
        tokens = word_tokenize(article_text)
        # Filter the stop words
        filtered_tokens = [token for token in tokens if token not in stop_words and len(token) >= 2]
        #Save the filtered tokens in a txt file
        text_file.write(' '.join(filtered_tokens) + "\n")
    
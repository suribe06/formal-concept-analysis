from unidecode import unidecode

namefile = "negative_words_es"

# Read the text file
with open(f"{namefile}.txt", "r", encoding="utf-8") as file:
    stop_words_es = file.readlines()

# Remove accents from the words
stop_words_es = [unidecode(word.strip()) for word in stop_words_es]

# Write the list of words to the file, one word per line
with open(f"{namefile}_2.txt", "w", encoding="utf-8") as file:
    file.writelines("\n".join(stop_words_es))


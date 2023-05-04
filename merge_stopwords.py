from nltk.corpus import stopwords

#stop_words_en = set(stopwords.words('english'))
#stop_words_spa = set(stopwords.words('spanish'))

with open('english.txt', 'r') as file1:
    words1 = file1.readlines()

with open('spanish.txt', 'r') as file2:
    words2 = file2.readlines()

words = words1 + words2
extra_words = ['https', 'jstor', 'org', 'doi', 'www', 'also', 'one', 'doi', 'downloaded', 'content', 
               'subject', 'wed', 'apr', 'utc', 'terms', 'use', 'university', 'press', 'united', 'states',
               'cuba', 'cuban', 'would', 'latin', 'america', 'american', 'first', 'well',  'however', 'two', 'many']

with open('stopwords.txt', 'w') as file3:
    file3.writelines(words)
    for word in extra_words:
        file3.write(word.strip() + '\n')
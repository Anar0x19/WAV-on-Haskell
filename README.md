Sound system on Haskell 

an analogue of a C program that works on a similar principle: it takes a WAV file as input – stereo, 16 bits per channel, approximately 20 seconds to 6 minutes long (i.e. less than 256 MB in size). It finds the largest “piece” of sound in it that is repeated in the same file – and cuts it out, returning a shorter file. 

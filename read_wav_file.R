library(av)

wav_file <- "~/230216_66150544-e985-4a97-823a-91a17e428a00.wav"
fft_data <- read_audio_fft(wav_file, end_time = 595.0)
plot(fft_data)

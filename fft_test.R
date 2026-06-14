library(av)

audio_file <- '/Volumes/TimChurchesSamsungT5_1TB/acoupi_and_thermal_tests_22_23_march_2026/2026/3/22/201247_aa99cac8-b643-409b-8efd-be9d0cb3c1c0.wav'

fft_data <- read_audio_fft(audio_file, start_time = 300.0, end_time = 360.0)

plot(fft_data)


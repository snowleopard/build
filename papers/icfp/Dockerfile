# Docker image prepared for ICFP'18 Artifact Evaluation.
#
# To build the image, run the following command in the directory containing
# this Dockerfile: `docker build -t snowleopard/build .`
#
# To run a container interactively:
# `docker run -it snowleopard/build`
#
FROM fpco/stack-build:lts-11.8
MAINTAINER Andrey Mokhov
RUN wget -O build-systems.zip https://github.com/snowleopard/build/archive/dbbe1322962cab3f523f61b7a6b3be57533cec44.zip
RUN unzip build-systems.zip
WORKDIR /build-dbbe1322962cab3f523f61b7a6b3be57533cec44
RUN stack build && stack test
RUN exit

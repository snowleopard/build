# Docker image prepared for ICFP'18 Artifact Evaluation.
#
# To build the image, run the following command in the directory containing
# this Dockerfile: `docker build -t snowleopard/build .`
#
# To run a container interactively:
# `docker run -it snowleopard/build`
#
FROM fpco/stack-build:lts-11
MAINTAINER Andrey Mokhov
RUN wget -O build-systems.zip https://github.com/snowleopard/build/archive/c72fb06429db8f4b168a7912b98b764bbb55e7e9.zip
RUN unzip build-systems.zip
WORKDIR /build-c72fb06429db8f4b168a7912b98b764bbb55e7e9
RUN stack build && stack test
RUN exit

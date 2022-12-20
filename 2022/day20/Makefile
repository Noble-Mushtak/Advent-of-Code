CXX=g++
CXXFLAGS=-g -O2 -std=gnu++17 -static -Wall

# https://stackoverflow.com/questions/3220277/what-do-the-makefile-symbols-and-mean
%: %.cpp Makefile
	$(CXX) $(CXXFLAGS) $< -o $@

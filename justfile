default: test build

run:
	sbt run

test:
	sbt test

build:
	sbt "graalvm-native-image:packageBin"

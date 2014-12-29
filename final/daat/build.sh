#!/bin/sh -v

#### From Environment Variables ####
smalijar=$SMALI_JAR

#### derived from adb's path ####
adb_path=`which adb`
adb_dir=`dirname $adb_path`
sdk_base="${adb_dir}/.."

aapt="${sdk_base}/build-tools/21.1.2/aapt"
zipalign="${sdk_base}/build-tools/21.1.2/zipalign"
sdklib="${sdk_base}/tools/lib/sdklib.jar"

# build-setup
mkdir -p bin
mkdir -p bin/res
mkdir -p bin/rsObj
mkdir -p bin/rsLibs
mkdir -p gen
mkdir -p bin/classes
mkdir -p bin/dexedLibs
mkdir -p src/

# Copy Source files to src/
cp DaatProg.s src/
cp src_lib/*.smali src/

# TODO: How to create AndroidManifest.xml?

# Merging Android Manifest file into one
$aapt package -f -m -M bin/AndroidManifest.xml \
      -S bin/res \
      -S res \
      -I ${sdk_base}/platforms/android-19/android.jar \
      -J gen \
      --generate-dependencies \
      -G bin/proguard.txt

# classes.dex
java -jar $smalijar -o bin/classes.dex src/*

# crunch
$aapt crunch -v -S res -C bin/res

# package-resource
$aapt package --no-crunch -f --debug-mode \
      -M bin/AndroidManifest.xml \
      -S bin/res \
      -S res \
      -I ${sdk_base}/platforms/android-19/android.jar \
      -F bin/Daat.ap_ \
      --generate-dependencies

# apkbuilder
ant

# zipalign
cd bin
$zipalign -f 4 Daat-debug-unaligned.apk Daat-debug.apk

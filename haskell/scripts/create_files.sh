#!/bin/bash

if [ "$#" -ne 1 ]; then
    echo "Usage: $0 <year>"
    exit 1
fi

year=$1

if ! [[ "$year" =~ ^[0-9]+$ ]]; then
    echo "Error: Year argument must be integer."
    exit 1
fi

########################
# CREATE MAIN APP FILE #
########################

main_dir="app/Year${year}"
mkdir -p $main_dir
main_file="$main_dir/Main.hs"

if [ ! -f "$main_file" ]; then
    echo "$main_file from template"
    awk -v year=$year '{
        gsub(/\$\{YEAR\}/, year);
        print;
    }' templates/YearXXXXMain.hs > "$main_file"
else
    echo "File $main_file already exists."
fi


#######################
# CREATE SOURCE FILES #
#######################
echo "Create source files"

src_dir="src/Year${year}"
mkdir -p $src_dir

for day in {1..25}; do
    padday=$(printf "%02d" "$day")
    new_file="${src_dir}/Day${padday}.hs"

    if [ ! -f "$new_file" ]; then
        echo "$new_file from template"
        awk -v year=$year -v day=$day -v padday=$padday '{
            gsub(/\$\{YEAR\}/, year);
            gsub(/\$\{DAY\}/, day);
            gsub(/\$\{PADDAY\}/, padday);
            print;
        }' templates/DayXX.hs > "$new_file"
    else
        echo "File $new_file already exists."
    fi
done


#####################
# CREATE TEST FILES #
#####################
test_dir="test/Tests/Year${year}"

mkdir -p $test_dir

new_file="${test_dir}.hs"
if [ ! -f "$new_file" ]; then
    echo "$new_file from template"
    awk -v year=$year -v day=$day -v padday=$padday '{
        gsub(/\$\{YEAR\}/, year);
        print;
    }' templates/YearXXXXTests.hs > "$new_file"
else
    echo "File $new_file already exists."
fi

for day in {1..25}; do
    padday=$(printf "%02d" "$day")
    new_file="${test_dir}/Day${padday}.hs"

    if [ ! -f "$new_file" ]; then
        echo "$new_file from template"
        awk -v year=$year -v day=$day -v padday=$padday '{
            gsub(/\$\{YEAR\}/, year);
            gsub(/\$\{DAY\}/, day);
            gsub(/\$\{PADDAY\}/, padday);
            print;
        }' templates/DayXXTests.hs > "$new_file"
    else
        echo "File $new_file already exists."
    fi
done


echo "Do not forget to add year $year to test/Spec.hs"
echo "Do not forget to add to package.yaml"
cat << EOF
  adventofcode${year}:
    main:                Main.hs
    source-dirs:         app/Year${year}
    ghc-options:
    - -threaded
    - -rtsopts
    - -with-rtsopts=-N
    - -Wall
    - -main-is Year${year}.Main
    dependencies:
    - adventofcode
EOF

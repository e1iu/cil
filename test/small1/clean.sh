#! /bin/bash
files=(*.c)
# clean
cleanup() {
  local i=0
  while ((i < ${#files[@]}));do
    file_name=${files[i]}
    dir_name=${file_name%.*}
    cmd="rm -rf "${dir_name}
    echo ${cmd}
    $(${cmd})
    ((i++))
  done
}

rm -rf *~
cleanup

#! /bin/bash

CILLY=~/Github/cil/bin/cilly

if [ $# -eq 1 ]; then
  files=$1
else
  files=(*.c)
fi

if [ -t 1 ]; then
  COLOR_RESET="\x1b[0m"
  COLOR_DEFAULT="\x1b[39;49;0m"
  COLOR_DEFAULT_BOLD="\x1b[39;49;1m"
  COLOR_RED_BOLD="\x1b[31;1m"
  COLOR_GREEN_BOLD="\x1b[32;1m"
  COLOR_YELLOW_BOLD="\x1b[33;1m"
  COLOR_MAGENTA_BOLD="\x1b[35;1m"
  COLOR_WHITE_ON_RED_BOLD="\x1b[37;41;1m"
else
  COLOR_RESET=""
  COLOR_DEFAULT=""
  COLOR_DEFAULT_BOLD=""
  COLOR_RED_BOLD=""
  COLOR_GREEN_BOLD=""
  COLOR_YELLOW_BOLD=""
  COLOR_MAGENTA_BOLD=""
  COLOR_WHITE_ON_RED_BOLD=""
fi

passed=0
failed=0
RUNTEST_SUCCESS=0
RUNTEST_FAILURE=1
RUNTEST_WARNING=2

print_success() {
  name=$1
  printf "%b[✓] PASSED:%b  %s%b\n" \
    "$COLOR_GREEN_BOLD" "$COLOR_DEFAULT" "$name" "$COLOR_RESET"
}

print_warning() {
  name=$1
  printf "%b[?] WARNING:%b  %s%b\n" \
    "$COLOR_YELLOW_BOLD" "$COLOR_DEFAULT" "$name" "$COLOR_RESET"
}


print_failure() {
  name=$1
  printf "%b[✗] FAILED:%b  %s%b\n" \
    "$COLOR_RED_BOLD" "$COLOR_DEFAULT" "$name" "$COLOR_RESET"
}

print_run() {
  name=$1
  printf "%b[ ] RUNNING:%b %s%b\r" \
    "$COLOR_DEFAULT_BOLD" "$COLOR_DEFAULT" "$name" "$COLOR_RESET"
}

# clean
cleanup() {
  local i=0
  while ((i < ${#files[@]}));do
    file_name=${files[i]}
    dir_name=${file_name%.*}
    rm -rf ${dir_name}
    ((i++))
  done
}

runtest() {
  name=$1
  prefix=${name%.*}
  mkdir ${prefix}
  cd ${prefix}
  gcc ../${name} -o ${prefix}".out" > gcc.log 2>&1
  if [ ! -f ${prefix}".out" ]; then
    return ${RUNTEST_WARNING}
  fi
  ${CILLY} ../${name} -o ${prefix}".cil.out" > cilly.log 2>&1
  if [ ! -f ${prefix}".cil.out" ]; then
    return ${RUNTEST_WARNING}
  fi
  r1="1.result"
  r2="2.result"
  ${CILLY} --doflatten --useLogicalOperators --save-temps ../${name} -o \
    ${prefix}".flatten.out" > flatten.log 2>&1

  gtimeout 3 ./${prefix}".cil.out" > ${r1} 2>&1
  gtimeout 3 ./${prefix}".flatten.out" > ${r2} 2>&1

  cmp ${r1} ${r2} > /dev/null 2>&1
  if [ $? -ne 0 ]; then
    return $RUNTEST_FAILURE
  else
    return $RUNTEST_SUCCESS
  fi
}

pids=()
num_to_run_in_parallel=4
next_test_index=0
next_test_to_reap=0

cleanup

start_test() {
  if (( next_test_index < ${#files[@]} )); then
    local name="${files[${next_test_index}]}"
    runtest "${name}" &
    pids[${next_test_index}]=$!
  fi
  (( next_test_index++ ))
}



for ignore_me in $(seq ${num_to_run_in_parallel}); do
  start_test
done

while ((next_test_to_reap < ${#files[@]})); do
  name=${files[next_test_to_reap]}
  print_run ${name}
  wait "${pids[next_test_to_reap]}"
  case $? in
    $RUNTEST_SUCCESS )
      (( passes++ ))
      print_success "${name}" ;;
    $RUNTEST_FAILURE )
      (( failed++ ))
      print_failure "${name}" ;;
    $RUNTEST_WARNING )
      print_warning "${name}" ;;
    *)
      echo "unknown error"
      ;;
  esac
  ((next_test_to_reap++))

  start_test
done

#!/bin/bash

# Build the compiler
echo "--- Building Compiler ---"
zig build

COMPILER="./zig-out/bin/compiler"

# Function to run a test
# Arguments: file, expected_exit_code, arch
run_test() {
    local file=$1
    local expected=$2
    local arch=$3

    echo "Testing $file ($arch)..."
    
    # Run compiler
    $COMPILER --arch $arch $file
    if [ $? -ne 0 ]; then
        echo "  [FAIL] Compilation failed"
        return 1
    fi

    # Assemble and link
    mv out.asm out.s
    clang -arch $arch -o out_bin out.s
    if [ $? -ne 0 ]; then
        echo "  [FAIL] Assembly/Linking failed"
        rm -f out.s
        return 1
    fi

    # Run and check exit code
    ./out_bin
    local actual=$?
    
    if [ $actual -eq $expected ]; then
        echo "  [PASS] Got $actual"
    else
        echo "  [FAIL] Expected $expected but got $actual"
        rm -f out.s out_bin
        return 1
    fi

    rm -f out.s out_bin
    return 0
}

# Run tests for both architectures
ARCHS=("arm64" "x86_64")
EXAMPLES=(
    "examples/loop.simple:45"
    "examples/if_else.simple:2"
    "examples/arithmetic.simple:10"
    "examples/nested.simple:9"
    "examples/new_features.simple:43"
    "examples/bitwise_compound.simple:43"
    "examples/functions.simple:11"
    "examples/pointers.simple:100"
    "examples/globals.simple:42"
    "examples/printf.simple:0"
    "examples/exit.simple:42"
    "examples/complex.simple:42"
    "examples/optimize.simple:51"
    "examples/sieve.simple:100"
    "examples/structs.simple:42"
    "examples/logical_precedence.simple:42"
    "examples/recursion_factorial.simple:42"
    "examples/array_sum.simple:42"
    "examples/global_init.simple:42"
    "examples/compound_ptr.simple:42"
    "examples/nested_logic.simple:42"
    "examples/char_test.simple:42"
    "examples/loop_control.simple:42"
    "examples/void_char.simple:42"
    "examples/typedef_enum.simple:42"
    "examples/nested_loops_complex.simple:42"
    "examples/postfix.simple:42"
    "examples/ternary.simple:42"
    "examples/switch.simple:42"
    "examples/bubble_sort.simple:42"
    "examples/binary_search.simple:42"
    "examples/linked_list.simple:42"
    "examples/bit_counting.simple:42"
    "examples/multi_decl.simple:42"
    "examples/sizeof.simple:42"
    "examples/preprocessor.simple:44"
    "examples/type_cast.simple:0"
    "examples/float_math.simple:0"
)

TOTAL_PASS=0
TOTAL_FAIL=0

for arch in "${ARCHS[@]}"; do
    echo "=== Architecture: $arch ==="
    for item in "${EXAMPLES[@]}"; do
        file="${item%%:*}"
        expected="${item##*:}"
        
        run_test "$file" "$expected" "$arch"
        if [ $? -eq 0 ]; then
            ((TOTAL_PASS++))
        else
            ((TOTAL_FAIL++))
        fi
    done
    echo ""
done

echo "--- Summary ---"
echo "Passed: $TOTAL_PASS"
echo "Failed: $TOTAL_FAIL"

if [ $TOTAL_FAIL -ne 0 ]; then
    exit 1
fi

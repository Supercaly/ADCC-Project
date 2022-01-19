function Install {
    New-Item -ItemType Directory -Force -Path ebin
    Copy-Item "src\ts_app.app.src" -Destination "ebin\ts_app.app"    
}

function Compile {
    $srcs = Get-ChildItem src\*.erl 
    foreach ($src in $srcs) {
        erlc -o ebin $src
    }
}

function Compile-Test {
    $srcs = Get-ChildItem src\*.erl 
    foreach ($src in $srcs) {
        erlc -o ebin -DTEST $src
    }
}

switch ($args)
{
    "install" { Install }
    "compile" { 
        Install
        Compile
    }
    "compile_test" { 
        Install
        Compile-Test
    }
    "test" { 
        Install
        Compile-Test
        ct_run -dir test -pa ebin
    }
    "clean" { git clean -fxd }
    default {
        Write-Error -Message "Unknown command '$args'. Accepted values: install/compile/compile_test/test/clean"
    }
}


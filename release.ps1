$release_dest = "./release/"
if (Test-Path $release_dest) {
    Remove-Item $release_dest -Recurse
}

if (Test-Path "/release.7zip") {
    Remove-Item "/release.7zip"
} 

lein --% do clean, javac, uberjar
New-Item -Path $release_dest -type directory
New-Item -Path ($release_dest + "/projects/") -type directory
Copy-Item -Path  "./release-template/*" -Destination $release_dest
Copy-Item -Path  "./target/urdmi-standalone.jar" -Destination ($release_dest + "/urdmi.jar")
Copy-Item -Path  "./dev-resources/projects/*" -Recurse -Destination ($release_dest + "/projects/")
Copy-Item -Path  "./dev-resources/aleph_projects/*" -Recurse  -Destination ($release_dest + "/projects/")
lein --% do clean, javac

C:\'Program Files'\7-Zip\7z.exe --% a ./release.7zip release
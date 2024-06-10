# Installing on Windows
1. Install Vagrant (https://developer.hashicorp.com/vagrant/install?product_intent=vagrant)
2. Install VirtualBox (https://www.oracle.com/kr/virtualization/technologies/vm/downloads/virtualbox-downloads.html)
3. Run 
```
vagrant up
vagrant ssh
```

# Installing on MacOS
1. Install homebrew (https://brew.sh)
2. Install opam (https://opam.ocaml.org/doc/Install.html)
```
brew install opam
opam init
eval $(opam env --switch=default)
```
3. Install dune, etc
```
opam install dune merlin ocaml-lsp-server odoc ocamlformat utop dune-release
```
If dune is not foud, add the following in .bash_profile (or .zprofile)
```
eval $(opam config env) 
```
4. Install z3, etc
```
opam install mtime z3 batteries core
```

# 환경 설정
안녕하세요, 소프트웨어 검증 조교 김동욱입니다.

현재 vagrant 작동에 어려움을 겪고 계신 분들이 있어서 Docker 환경을 제공하려고 합니다.

1.
    - Docker [link]
    - VSCode [link]
    - Dev Containers (vscode extension) [link]

위의 프로그램들을 설치한 후,

    git clone https://github.com/kupl-courses/COSE419-2024.git

    mkdir COSE419-2024/.devcontainer; touch COSE419-2024/.devcontainer/devcontainer.json

    echo "{\"name\": \"cose419-2024\", \"image\": \"ghcr.io/sambyeol/ocaml-devcontainer\", \"customizations\": {\"vscode\": {\"extensions\": [\"ocamllabs.ocaml-platform\"]}}}" > COSE419-2024/.devcontainer/devcontainer.json

    code COSE419-2024

해당 명령어를 통해 vscode를 실행하시면 됩니다.



2.

만약 docker가 실행 중이 아니라면 먼저 docker를 실행합니다.

vscode에서 명령어 팔레트를 열어(MacOS: Shift+Command+P / Window: Ctrl+Shift+P),

    Dev Containers: Reopen in Container

해당 명령어를 통해 docker 환경을 빌드하시면 됩니다.



3.

docker 환경이 빌드가 완료되었다면,

    opam install mtime z3 logs batteries core

를 통해 모듈들을 설치해주시면 문제 없이 과제를 진행하실 수 있습니다.

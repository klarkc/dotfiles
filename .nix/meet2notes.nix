{
  lib,
  python312Packages,
  makeWrapper,
  ffmpeg,
  codex,
  stdenv,
  meet2notes-src,
}:

let
  tokenizersLite = python312Packages.tokenizers.overridePythonAttrs (_: {
    doCheck = false;
    nativeCheckInputs = [ ];
  });
  ctranslate2Lite = python312Packages.ctranslate2.overridePythonAttrs (old: {
    doCheck = false;
    nativeCheckInputs = lib.filter (
      dep:
      !builtins.elem (dep.pname or "") [
        "torch"
        "transformers"
        "wurlitzer"
      ]
    ) old.nativeBuildInputs;
  });
  fasterWhisperLite = python312Packages.faster-whisper.overridePythonAttrs (old: {
    dependencies = map (
      dep:
      if (dep.pname or "") == "tokenizers" then
        tokenizersLite
      else if (dep.pname or "") == "ctranslate2" then
        ctranslate2Lite
      else
        dep
    ) old.dependencies;
  });
in
python312Packages.buildPythonApplication rec {
  pname = "meet2notes";
  version = "0.6.2-klarkc";
  pyproject = true;

  src = meet2notes-src;
  patches = [ ./patches/meet2notes-codex-oauth.patch ];

  build-system = with python312Packages; [
    hatchling
  ];

  dependencies = with python312Packages; [
    aiofiles
    fastapi
    httpx
    jinja2
    keyring
    mcp
    numpy
    platformdirs
    pydantic
    pydantic-settings
    python-multipart
    sherpa-onnx
    fasterWhisperLite
    uvicorn
  ];

  pythonRemoveDeps = [
    "fastembed"
    "litellm"
  ];

  nativeBuildInputs = [ makeWrapper ];

  postInstall = ''
    for program in meet2notes meet2notes-models; do
      wrapProgram "$out/bin/$program" \
        --prefix PATH : ${lib.makeBinPath [ ffmpeg codex ]} \
        --prefix LD_LIBRARY_PATH : ${stdenv.cc.cc.lib}/lib \
        --set-default MEET2NOTES_AI_API_KEY hackme
    done
  '';

  pythonImportsCheck = [ "local_meeting_ai" ];
  doCheck = false;

  meta = {
    description = "Private local meeting transcription, diarization, and AI summaries";
    homepage = "https://meet2notes.eu";
    license = lib.licenses.mit;
    mainProgram = "meet2notes";
    platforms = lib.platforms.linux;
  };
}

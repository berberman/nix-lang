# Blank lines inside interpolations must be preserved.
{
  postInstall = ''
    --prefix PATH : ${

      lib.makeBinPath [
        pkgs.fzf
      ]
    }
  '';
}

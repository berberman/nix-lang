let
  apply = a b + 1;
  assert_ = assert a == 1; 2;
  s = {
    m = 3;
    g = 4;
  };
in {
  inherit apply;
  inherit (s) m g;
  inherit "m";
  inherit ${"k"};
  inherit (g);
  inherit;
  a.b.c = 1;
  apply2 = a b c d e f;
  float = 2.33;
  "${dynamic}".${attr} = g;
  empty_set = { };
  rec_set = rec { k = 233; };
  bool_arith = a >= b || c < d && e == f;
  bool = false -> !true || true && false;
  # comment f
  f =
    x:
    # comment y
    y:
    x;
  has_attr = a ? b && c;
  if_else = if a then b else if c then d else e;
  import_nix = import <nixpkgs> { };
  interpol_d = "${qwq}";
  interpol_ds = ''
    ${qwq}
    ${{x = 1;}.x}
  '';
  lambda1 = _:233;
  lambda2 = x:[x];
  lambda3 = x: y: x;
  list = [1 2 3 "4" false k] ++ [l];
  arith = 1 / 2 + (3 + 4) + -5;
  merge = {x = 1;} // {x = 2;};
  path_1 = 1/2;
  # unsupported for now
  #
  # op_no_ws_ = 1*-23;
  # multi_has = {} ? a ? b ? c;
  # legacy_let = let { x = 1; body = x; };
  # floating = .1;
  comment_no_ws = [a/**/];
  or_is_ident = a b or c;
  or = 1;
  x.or = 1;
  path_root = /nix;
  path_home = ~/nix;
  path_env = <nixpkgs/nixos>;
  path_cur = ./nix;
  path_interpol_1 = ./${a}-${b}/c/d${e};
  path_interpol_2 = c/d${e};
  as_left = a@{x}: x;
  as_right = {x}@a: x;
  param = {x ? 1, y ? {}, ...}: x;
  param_comma = {x,}: x;
  sel_or = a.b.c or d.e;
  sel_interpol = a.b.${c}."d"."${"e"}";
  uri = https://www.example.com;
  w = with l; 1;
}

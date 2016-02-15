load(models).
% ace engine settings
% file appended to the generated .s file
use_package(icl).
use_package(query).
load(models).

classes([grandfather_yes, grandfather_no]).

icl_multi(on).

significance_level(0).
talking(4).

simplify(on).

maxhead(3).
maxbody(10).
# Ott doesn't know how to deal with substitution in the phantom selem production
s/-> RegVal_Struct selem_list/-> RegVal_Struct (List.map (fun (name,valu) -> (name, subst_val_valu base_val5 vvar5 valu)) selem_list)/
s/=> RegVal_Struct selem_list/=> RegVal_Struct (map (fun '(name,valu) => (name, subst_val_valu base_val_6 vvar5 valu)) selem_list)/


# parsetab.py
# This file is automatically generated. Do not edit.
_tabversion = '3.8'

_lr_method = 'LALR'

_lr_signature = 'C9A024CB30CB4E73621C1C0BD8B585C1'
    
_lr_action_items = {'QUOTE':([0,2,3,5,7,8,10,11,14,15,16,17,18,19,20,21,25,26,],[1,-15,-16,-19,-17,-14,-21,-20,1,-4,1,1,-10,-12,-11,-9,-5,-13,]),'$end':([0,2,3,5,6,7,8,9,10,11,12,13,15,25,26,],[-18,-15,-16,-19,-2,-17,-14,-3,-21,-20,0,-1,-4,-5,-13,]),'NUM':([0,2,3,5,7,8,10,11,14,15,16,17,18,19,20,21,25,26,],[3,-15,-16,-19,-17,-14,-21,-20,3,-4,3,3,-10,-12,-11,-9,-5,-13,]),'LPAREN':([0,1,2,3,5,7,8,10,11,14,15,16,17,18,19,20,21,25,26,],[4,14,-15,-16,-19,-17,-14,-21,-20,4,-4,4,4,-10,-12,-11,-9,-5,-13,]),'TRUE':([0,2,3,5,7,8,10,11,14,15,16,17,18,19,20,21,25,26,],[5,-15,-16,-19,-17,-14,-21,-20,5,-4,5,5,-10,-12,-11,-9,-5,-13,]),'TEXT':([0,2,3,5,7,8,10,11,14,15,16,17,18,19,20,21,25,26,],[7,-15,-16,-19,-17,-14,-21,-20,7,-4,7,7,-10,-12,-11,-9,-5,-13,]),'RPAREN':([2,3,5,7,8,10,11,14,15,16,17,18,19,20,21,22,23,24,25,26,],[-15,-16,-19,-17,-14,-21,-20,-8,-4,-8,-8,-10,-7,-11,-9,25,26,-6,-5,-13,]),'SIMB':([0,2,3,4,5,7,8,10,11,14,15,16,17,18,19,20,21,25,26,],[8,-15,-16,16,-19,-17,-14,-21,-20,8,-4,8,8,-10,-12,-11,-9,-5,-13,]),'NIL':([0,2,3,5,7,8,10,11,14,15,16,17,18,19,20,21,25,26,],[10,-15,-16,-19,-17,-14,-21,-20,10,-4,10,10,-10,-12,-11,-9,-5,-13,]),'FALSE':([0,2,3,5,7,8,10,11,14,15,16,17,18,19,20,21,25,26,],[11,-15,-16,-19,-17,-14,-21,-20,11,-4,11,11,-10,-12,-11,-9,-5,-13,]),}

_lr_action = {}
for _k, _v in _lr_action_items.items():
   for _x,_y in zip(_v[0],_v[1]):
      if not _x in _lr_action:  _lr_action[_x] = {}
      _lr_action[_x][_k] = _y
del _lr_action_items

_lr_goto_items = {'item':([14,16,17,],[17,17,17,]),'bool':([0,14,16,17,],[2,2,2,2,]),'quoted_list':([0,14,16,17,],[6,18,18,18,]),'list':([1,],[15,]),'empty':([14,16,17,],[19,19,19,]),'call':([0,14,16,17,],[9,20,20,20,]),'exp':([0,],[12,]),'atom':([0,14,16,17,],[13,21,21,21,]),'items':([14,16,17,],[22,23,24,]),}

_lr_goto = {}
for _k, _v in _lr_goto_items.items():
   for _x, _y in zip(_v[0], _v[1]):
       if not _x in _lr_goto: _lr_goto[_x] = {}
       _lr_goto[_x][_k] = _y
del _lr_goto_items
_lr_productions = [
  ("S' -> exp","S'",1,None,None,None),
  ('exp -> atom','exp',1,'p_exp_atom','yacc.py',162),
  ('exp -> quoted_list','exp',1,'p_exp_qlist','yacc.py',166),
  ('exp -> call','exp',1,'p_exp_call','yacc.py',170),
  ('quoted_list -> QUOTE list','quoted_list',2,'p_quoted_list','yacc.py',174),
  ('list -> LPAREN items RPAREN','list',3,'p_list','yacc.py',178),
  ('items -> item items','items',2,'p_items','yacc.py',182),
  ('items -> empty','items',1,'p_items_empty','yacc.py',186),
  ('empty -> <empty>','empty',0,'p_empty','yacc.py',190),
  ('item -> atom','item',1,'p_item_atom','yacc.py',194),
  ('item -> quoted_list','item',1,'p_item_list','yacc.py',202),
  ('item -> call','item',1,'p_item_call','yacc.py',206),
  ('item -> empty','item',1,'p_item_empty','yacc.py',210),
  ('call -> LPAREN SIMB items RPAREN','call',4,'p_call','yacc.py',214),
  ('atom -> SIMB','atom',1,'p_atom_simbol','yacc.py',231),
  ('atom -> bool','atom',1,'p_atom_bool','yacc.py',237),
  ('atom -> NUM','atom',1,'p_atom_num','yacc.py',241),
  ('atom -> TEXT','atom',1,'p_atom_word','yacc.py',245),
  ('atom -> <empty>','atom',0,'p_atom_empty','yacc.py',249),
  ('bool -> TRUE','bool',1,'p_true','yacc.py',253),
  ('bool -> FALSE','bool',1,'p_false','yacc.py',258),
  ('atom -> NIL','atom',1,'p_nil','yacc.py',263),
]

Caml1999N029����            7src/lemonade_Reader.rei����  !~  ^  �  ࠠ���*ocaml.text��7src/lemonade_Reader.reiN���U@M@�������
  p Reader monad.

    Values in a {i Reader} monad or {i Environment monad} represent a
    computation, which can read values from a shared environment, pass
    values from function to function, and execute sub-computations in
    a modified environment. Using a {i Reader} monad for such
    computations is often clearer and easier than using a {i State}
    monad. ��&_none_@@ ��@@ �A@��N���U@M@@@@��N���U@M@@��N���U@M@��N���U@M@�����/EnvironmentType��"Y���#Y��@������A�    �!t��/\���0\��@@@@A@���)ocaml.doc��6Z���7Z��@�������	. The type of data consumed in a reader monad. ��5@@ ��6@@ �A@��DZ���EZ��@@@@��GZ���HZ��@@��JZ���KZ��@@��M\���N\��@@��PZ���Q\��@@��SY���T]��@@���$��YWPP�ZWP�@�������	< The input signature of the functor [Lemonade_Reader.Make]. ��X@@ ��Y@@ �A@��gWPP�hWP�@@@@��jWPP�kWP�@@��mWPP�nWP�@@��pWPP�q]��@��sWPP�t]��@�����!S��|a<H�}a<I@������A�    �+environment���dsz��ds�@@@@A@���Z���bNP��bNq@�������< The type of consumed data. ���@@ ���@@ �A@���bNP��bNq@@@@���bNP��bNq@@���bNP��bNq@@���dsu��ds�@@���bNP��ds�@���������-Lemonade_Type!S���f����f��@���f����f��@@���f����f��@@���f����f��@���Р$read���j����j��@����!t���j����j��@�����+environment���j����j��@@���j����j��@@@@���j����j��@@@@�������h����h��@�������	! Access the current environment. ���@@ ���@@ �A@���h����h��@@@@���h����h��@@���h����h��@@���j����j��@�� h���j��@���Р#run��	n,2�
n,5@��@����+environment��n,8�n,C@@��n,8�n,C@@@��@����!t�� n,E�!n,F@���!a��'n,G�(n,I@@@@��*n,E�+n,J@@@��!a��0n,O�1n,Q@@@��3n,E�4n,Q@@@��6n,7�7n,Q@@@@�����<l���=l�*@�������	8 Perform a computation in the given environment errors. ��;@@ ��<@@ �A@��Jl���Kl�*@@@@��Ml���Nl�*@@��Pl���Ql�*@@��Sn,.�Tn,Q@��Vl���Wn,Q@���Р%local��_r���`r��@��@��@����+environment��kr���lr��@@��nr���or��@@@����+environment��vr���wr��@@��yr���zr��@@@��|r���}r��@@@��@����!t���r����r��@���!a���r����r��@@@@���r����r��@@@����!t���r����r��@���!a���r����r��@@@@���r����r��@@@���r����r��@@@���r����r��@@@@���y���pTV��pT�@�������	2 Execute a computation in a modified environment. ���@@ ���@@ �A@���pTV��pT�@@@@���pTV��pT�@@���pTV��pT�@@���r����r��@���pTV��r��@���Р&access���v��v@��@��@����+environment���v��v!@@���v��v!@@@��!a���v%��v'@@@���v��v'@@@����!t���v,��v-@���!a���v.��v0@@@@���v,��v1@@@���v��v1@@@@���ϰ�t���t�@�������	3 Access to a component of the current environment. ��@@ ��@@ �A@��t���t�@@@@��t���t�@@��t���t�@@��v	�v1@��t���v1@@��!a<L�"w34@@�����'_���(_�:@�������	= The output signature of the functor [Lemonade_Reader.Make]. ��&@@ ��'@@ �A@��5_���6_�:@@@@��8_���9_�:@@��;_���<_�:@@��>_���?w34@��A_���Bw34@������$Make��K{z��L{z�@�����+Environment��T|���U|��@����/EnvironmentType��\|���]|��@��_|���`|��@@������������!S��o~���p~��@��r~���s~��@@����+environment��z~���{~��@    ���~����~��@@@@A������+Environment!t���~����~��@@���~����~��@@@@���~����~��@@���~����~��@@���~����~��@@���~����~��@������!T��� B�� B@�����!M��� C&�� C'@�����-Lemonade_Type!S��� C)�� C8@��� C)�� C8@@�����A�    �+environment��� Gq~�� Gq�@@@@A������+Environment!t��� Gq��� Gq�@@��� Gq��� Gq�@@@������� EFN�� EFo@�������< The type of consumed data. ���@@ ���@@ �A@��� EFN�� EFo@@@@��� EFN�� EFo@@��� EFN�� EFo@@��� Gqy�� Gq�@@��� EFN�� Gq�@�����������-Lemonade_Type!S�� I��� I��@�� I��� I��@@����!t�� I��� I��@    ��� I��� I��@����!a�� I��� I��@@@�BA@@@A�����!t��% I���& I��@������!M!t��0 I���1 I��@���!a��7 I���8 I��@@@@��: I���; I��@@@@��= I���> I��@@@@��@ I���A I��@@��C I���D I��@@��F I���G I��@@��I I���J I��@���Р$read��R M�S M@����!t��Z M�[ M@�����+environment��c M�d M*@@��f M�g M*@@@@��i M�j M+@@@@���:��o K���p K�	@�������	! Access the current environment. ��n@@ ��o@@ �A@��} K���~ K�	@@@@��� K���� K�	@@��� K���� K�	@@��� M�� M+@��� K���� M+@���Р#run��� Qu��� Qu�@��@����+environment��� Qu��� Qu�@@��� Qu��� Qu�@@@��@����!t��� Qu��� Qu�@���!a��� Qu��� Qu�@@@@��� Qu��� Qu�@@@�����!M!t��� Qu��� Qu�@���!a��� Qu��� Qu�@@@@��� Qu��� Qu�@@@��� Qu��� Qu�@@@��� Qu��� Qu�@@@@������� O.6�� O.s@�������	8 Perform a computation in the given environment errors. ���@@ ���@@ �A@��� O.6�� O.s@@@@��� O.6�� O.s@@��� O.6�� O.s@@��� Qu}�� Qu�@��� O.6�� Qu�@���Р%local��� U���� U��@��@��@����+environment�� U��� U�	@@�� U��� U�	@@@����+environment�� U�	� U�	@@�� U�	� U�	@@@�� U��� U�	@@@��@����!t�� U�	� U�	@���!a��$ U�	�% U�	@@@@��' U�	�( U�	@@@����!t��/ U�	#�0 U�	$@���!a��6 U�	%�7 U�	'@@@@��9 U�	#�: U�	(@@@��< U�	�= U�	(@@@��? U���@ U�	(@@@@�����E S���F S��@�������	2 Execute a computation in a modified environment. ��D@@ ��E@@ �A@��S S���T S��@@@@��V S���W S��@@��Y S���Z S��@@��\ U���] U�	(@��_ S���` U�	(@���Р&access��h Y	m	y�i Y	m	@��@��@����+environment��t Y	m	��u Y	m	�@@��w Y	m	��x Y	m	�@@@��!a��} Y	m	��~ Y	m	�@@@��� Y	m	��� Y	m	�@@@����!t��� Y	m	��� Y	m	�@���!a��� Y	m	��� Y	m	�@@@@��� Y	m	��� Y	m	�@@@��� Y	m	��� Y	m	�@@@@���f��� W	+	3�� W	+	k@�������	3 Access to a component of the current environment. ���@@ ���@@ �A@��� W	+	3�� W	+	k@@@@��� W	+	3�� W	+	k@@��� W	+	3�� W	+	k@@��� Y	m	u�� Y	m	�@��� W	+	3�� Y	m	�@���Р$lift��� ]	�	��� ]	�	�@��@�����!M!t��� ]	�	��� ]	�	�@���!a��� ]	�	��� ]	�	�@@@@��� ]	�	��� ]	�	�@@@����!t��� ]	�	��� ]	�	�@���!a��� ]	�	��� ]	�
@@@@��� ]	�	��� ]	�
@@@��� ]	�	��� ]	�
@@@@������� [	�	��� [	�	�@�������	1 Add an environment to a monad of type ['a M.t]. ���@@ ���@@ �A@��� [	�	��� [	�	�@@@@��  [	�	�� [	�	�@@�� [	�	�� [	�	�@@�� ]	�	�� ]	�
@��	 [	�	��
 ]	�
@@�� D=D� ^

@@�� C%� ^

@@����� @��� @�@�������	  The success monad transformer. ��@@ ��@@ �A@��# @���$ @�@@@@��& @���' @�@@��) @���* @�@@��, @���- ^

@��/ @���0 ^

@@��2}���3 _

@@��5|���6 _

@@�����;y77�<y7x@�������	< Functor building an implementation of the [Success] monad. ��:@@ ��;@@ �A@��Iy77�Jy7x@@@@��Ly77�My7x@@��Oy77�Py7x@@��Ry77�S _

@��Uy77�V _

@@
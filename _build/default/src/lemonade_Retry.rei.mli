Caml1999N029����            6src/lemonade_Retry.rei����  I  �  �  Ӡ����*ocaml.text��6src/lemonade_Retry.reiN���V;@�������
  h Retry monad.

    The retry monad is a monad in which one can run computations
    throwing errors, which can be handled by retry strategies
    according to a policy. This is implemented as a functor
    parametrised by the error type and the policy.

    Note that the [throw] and [catch] operations defined below are
    totally independant of exceptions. ��&_none_@@ ��@@ �A@��N���V;@@@@��N���V;@@��N���V;@��N���V;@�����)RetryType��"Z���#Z��@������A�    �%error��/]���0]��@@@@A@���)ocaml.doc��6[���7[��@�������= The type of error messages. ��5@@ ��6@@ �A@��D[���E[��@@@@��G[���H[��@@��J[���K[��@@��M]���N]��@@��P[���Q]��@���A�    �#tag��Za�[a@@@@A@���+��`_���a_�@�������	0 The type of tags identifying retry strategies. ��_@@ ��`@@ �A@��n_���o_�@@@@��q_���r_�@@��t_���u_�@@��wa
�xa@@��z_���{a@���A�    �+environment���eDK��eDV@@@@A@���U���c��cB@�������	& The type of computation environment. ���@@ ���@@ �A@���c��cB@@@@���c��cB@@���c��cB@@���eDF��eDV@@���c��eDV@���Р&policy���l����l��@��@����$list���l����l�@��������#tag���l���l�@@���l���l�@@@���!a���l�
��l�@@@@���l���l�@@@@���l����l�@@@��@����+environment���l���l�@@���l���l�@@@����$list���l� ��l�$@��������#tag���l�&��l�)@@���l�&��l�)@@@���!a���l�+��l�-@@@@��l�%�l�.@@@@��l� �l�/@@@��l��l�/@@@��
l���l�/@@@@���۰�gY[�j��@�������	� The type of policies, used to select a retry strategy.

      The policy filters applyable retry strategies under a given
      environment. ��@@ ��@@ �A@��gY[�j��@@@@��!gY[�"j��@@��$gY[�%j��@@��'l���(l�/@��*gY[�+l�/@@��-Z���.m12@@������3X>>�4X>~@�������	; The input signature of the functor [Lemonade_Retry.Make]. ��2@@ ��3@@ �A@��AX>>�BX>~@@@@��DX>>�EX>~@@��GX>>�HX>~@@��JX>>�Km12@��MX>>�Nm12@�����!S��Vqx��Wqx�@������A�    �%error��ct���dt��@@@@A@���4��ir���jr��@�������= The type of error messages. ��h@@ ��i@@ �A@��wr���xr��@@@@��zr���{r��@@��}r���~r��@@���t����t��@@���r����t��@���A�    �#tag���x����x�@@@@A@���^���v����v��@�������	0 The type of tags identifying retry strategies. ���@@ ���@@ �A@���v����v��@@@@���v����v��@@���v����v��@@���x����x�@@���v����x�@���A�    �+environment���|4;��|4F@@@@A@�������z��z2@�������	& The type of computation environment. ���@@ ���@@ �A@���z��z2@@@@���z��z2@@���z��z2@@���|46��|4F@@���z��|4F@���A�    �'outcome��� @}��� @}�@����!a��� @}��� @}�@@@�@A@@��Р'Success��� A���� A��@����!a��� A���� A��@@@@@��� A���� A��@@�Р%Error�� B��� B��@������%error�� B��� B��@@�� B��� B��@@@@@�� B��� B��@@@A@�����~IK�~I{@�������	+ The type of computations throwing errors. ��@@ ��@@ �A@��(~IK�)~I{@@@@��+~IK�,~I{@@��.~IK�/~I{@@��1 @}�2 B��@@��4~IK�5 B��@���������-Lemonade_Type!S��B D���C D��@��E D���F D��@@��H D���I D��@@��K D���L D��@���Р%throw��T H���U H�@��@����%error��^ H��_ H�	@@��a H��b H�	@@@����!t��i H��j H�@���!a��p H��q H�@@@@��s H��t H�@@@��v H��w H�@@@@���G��| F���} F��@�������8 Throw the given error. ��{@@ ��|@@ �A@��� F���� F��@@@@��� F���� F��@@��� F���� F��@@��� H���� H�@��� F���� H�@���Р%catch��� M���� M��@��@����!t��� M���� M��@���!a��� M���� M��@@@@��� M���� M��@@@��@��@����%error��� M���� M��@@��� M���� M��@@@����!t��� M���� M��@���!a��� M���� M��@@@@��� M���� M��@@@��� M���� M��@@@����!t��� M���� M��@���!a��� M���� M��@@@@��� M���� M��@@@��� M���� M��@@@��� M���� M��@@@@������� J�� KY�@�������	z [catch m handler] is a monad containing the same value as [m]
      and thrown errors are interepreted by the [handler]. ���@@ ���@@ �A@�� J� KY�@@@@�� J� KY�@@��	 J�
 KY�@@�� M��� M��@�� J� M��@���Р%retry�� RNT� RNY@��@����#tag��" RN\�# RN_@@��% RN\�& RN_@@@��@��@����+environment��1 RNa�2 RNl@@��4 RNa�5 RNl@@@����!t��< RNp�= RNq@���!a��C RNr�D RNt@@@@��F RNp�G RNu@@@��I RNa�J RNu@@@��@����!t��S RNw�T RNx@���!a��Z RNy�[ RN{@@@@��] RNw�^ RN|@@@����!t��e RN��f RN�@���!a��l RN��m RN�@@@@��o RN��p RN�@@@��r RNw�s RN�@@@��u RNa�v RN�@@@��x RN[�y RN�@@@@���I��~ O��� PL@�������	} [retry tag strategy m] compute the same value as [m] having the
      chance let the retry policy use [strategy] on errors. ��}@@ ��~@@ �A@��� O���� PL@@@@��� O���� PL@@��� O���� PL@@��� RNP�� RN�@��� O���� RN�@���Р#run��� V���� V��@��@����+environment��� V���� V��@@��� V���� V��@@@��@����!t��� V���� V��@���!a��� V���� V��@@@@��� V���� V��@@@����'outcome��� V���� V��@���!a��� V���� V��@@@@��� V���� V��@@@��� V���� V��@@@��� V���� V��@@@@������� T���� T��@�������	& Run the given retryable computation. ���@@ ���@@ �A@��� T���� T��@@@@��� T���� T��@@��� T���� T��@@��� V���� V��@��� T���� V��@@���qx��� W��@@���ΰ�o55�o5v@�������	< The output signature of the functor [Lemonade_Retry.Make]. ��@@ ��@@ �A@��o55�o5v@@@@��o55�o5v@@��o55�o5v@@��o55� W��@��o55� W��@������$Make��' [	-	4�( [	-	8@�����%Retry��0 \	:	=�1 \	:	B@����)RetryType��8 \	:	D�9 \	:	M@��; \	:	D�< \	:	M@@������!S��E ^	S	W�F ^	S	X@��H ^	S	W�I ^	S	X@@����%error��P _	^	i�Q _	^	n@    ���U _	^	i�V _	^	n@@@@A������%Retry%error��` _	^	q�a _	^	|@@��c _	^	q�d _	^	|@@@@��f _	^	d�g _	^	|@����#tag��n `	�	��o `	�	�@    ���s `	�	��t `	�	�@@@@A������%Retry#tag��~ `	�	�� `	�	�@@��� `	�	��� `	�	�@@@@��� `	�	��� `	�	�@����+environment��� a	�	��� a	�	�@    ���� a	�	��� a	�	�@@@@A������%Retry+environment��� a	�	��� a	�	�@@��� a	�	��� a	�	�@@@@��� a	�	��� a	�	�@@��� ^	S	W�� a	�	�@@��� \	:	<�� a	�	�@@���y��� Y���� Y�	+@�������	: Functor building an implementation of the [Retry] monad. ���@@ ���@@ �A@��� Y���� Y�	+@@@@��� Y���� Y�	+@@��� Y���� Y�	+@@��� Y���� a	�	�@��� Y���� a	�	�@@
module Arkham.Asset.Assets.MaskOfSilenusFaceOfTheVoid1 (maskOfSilenusFaceOfTheVoid1) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.ChaosBagStepState
import Arkham.ChaosToken.Types
import Arkham.Helpers.Window (getDrawSource)
import Arkham.Matcher
import Arkham.Window qualified as Window

newtype MaskOfSilenusFaceOfTheVoid1 = MaskOfSilenusFaceOfTheVoid1 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

maskOfSilenusFaceOfTheVoid1 :: AssetCard MaskOfSilenusFaceOfTheVoid1
maskOfSilenusFaceOfTheVoid1 = asset MaskOfSilenusFaceOfTheVoid1 Cards.maskOfSilenusFaceOfTheVoid1

instance HasAbilities MaskOfSilenusFaceOfTheVoid1 where
  getAbilities (MaskOfSilenusFaceOfTheVoid1 a) =
    [controlled_ a 1 $ triggered (WouldRevealChaosToken #when You) $ assetUseCost a Charge 1]

instance RunMessage MaskOfSilenusFaceOfTheVoid1 where
  runMessage msg a@(MaskOfSilenusFaceOfTheVoid1 attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (getDrawSource -> drawSource) _ -> do
      checkWhen $ Window.WouldRevealChaosTokens drawSource iid
      push
        $ ReplaceCurrentDraw drawSource iid
        $ Choose (toSource attrs) 1 ResolveChoice [Undecided Draw, Undecided Draw] [] Nothing
      cancelledOrIgnoredCardOrGameEffect (attrs.ability 1)
      pure a
    ChaosTokenSelected iid (isSource attrs -> True) token -> do
      when (isSymbolChaosToken token.face) do
        assignHorror iid (attrs.ability 1) 1
      pure a
    _ -> MaskOfSilenusFaceOfTheVoid1 <$> liftRunMessage msg attrs

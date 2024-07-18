module Arkham.Asset.Cards.PressPass2 (pressPass2, PressPass2 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Matcher
import Arkham.Modifier
import Arkham.Token

newtype PressPass2 = PressPass2 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

pressPass2 :: AssetCard PressPass2
pressPass2 = asset PressPass2 Cards.pressPass2

-- After you spend 1 or more clues or place 1 or more clues on your location, exhaust Press Pass:
instance HasAbilities PressPass2 where
  getAbilities (PressPass2 a) =
    [ restrictedAbility a 1 ControlsThis
        $ ReactionAbility
          ( oneOf
              [ SpentClues #after You (atLeast 1)
              , PlacedToken #after (SourceOwnedBy You) (LocationTargetMatches YourLocation) Clue
              ]
          )
          (exhaust a)
    ]

instance RunMessage PressPass2 where
  runMessage msg a@(PressPass2 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      isTurn <- iid <=~> TurnInvestigator
      if isTurn
        then push $ GainActions iid (attrs.ability 1) 1
        else turnModifier iid (attrs.ability 1) iid (AdditionalActions "Press Pass" (attrs.ability 1) 1)
      pure a
    _ -> PressPass2 <$> liftRunMessage msg attrs

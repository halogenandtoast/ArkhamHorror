module Arkham.Asset.Assets.PennyWhite (pennyWhite) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Modifiers (controllerGets, pattern SkillModifier)
import Arkham.Matcher

newtype PennyWhite = PennyWhite AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

pennyWhite :: AssetCard PennyWhite
pennyWhite = ally PennyWhite Cards.pennyWhite (3, 2)

instance HasModifiersFor PennyWhite where
  getModifiersFor (PennyWhite a) = controllerGets a [SkillModifier #willpower 1]

instance HasAbilities PennyWhite where
  getAbilities (PennyWhite x) =
    [ controlled x 1 (CanDiscoverCluesAt YourLocation <> OnLocation LocationWithAnyClues)
        $ triggered (SkillTestResult #after You SkillTestFromRevelation #success) (exhaust x)
    ]

instance RunMessage PennyWhite where
  runMessage msg a@(PennyWhite attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      discoverAtYourLocation NotInvestigate iid (attrs.ability 1) 1
      pure a
    _ -> PennyWhite <$> liftRunMessage msg attrs

module Arkham.Asset.Cards.RuthWestmacottDarkRevelations (
  ruthWestmacottDarkRevelations,
  RuthWestmacottDarkRevelations (..),
)
where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Matcher
import Arkham.Modifier

newtype RuthWestmacottDarkRevelations = RuthWestmacottDarkRevelations AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ruthWestmacottDarkRevelations :: AssetCard RuthWestmacottDarkRevelations
ruthWestmacottDarkRevelations = ally RuthWestmacottDarkRevelations Cards.ruthWestmacottDarkRevelations (2, 3)

instance HasAbilities RuthWestmacottDarkRevelations where
  getAbilities (RuthWestmacottDarkRevelations attrs) =
    [ restrictedAbility attrs 1 ControlsThis
        $ ReactionAbility
          ( InitiatedSkillTest #when (affectsOthers Anyone) #any #any
              $ oneOf [SkillTestWithAction #fight, SkillTestWithAction #evade, SkillTestOnEncounterCard]
          )
          GloriaCost
    ]

instance RunMessage RuthWestmacottDarkRevelations where
  runMessage msg a@(RuthWestmacottDarkRevelations attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      skillTestModifier (attrs.ability 1) SkillTestTarget (Difficulty (-2))
      pure a
    _ -> RuthWestmacottDarkRevelations <$> liftRunMessage msg attrs

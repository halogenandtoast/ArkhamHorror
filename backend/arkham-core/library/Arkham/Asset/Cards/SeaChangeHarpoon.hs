module Arkham.Asset.Cards.SeaChangeHarpoon (
  seaChangeHarpoon,
  SeaChangeHarpoon (..),
)
where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Card
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Projection

newtype SeaChangeHarpoon = SeaChangeHarpoon AssetAttrs
  deriving anyclass (IsAsset)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

seaChangeHarpoon :: AssetCard SeaChangeHarpoon
seaChangeHarpoon =
  asset SeaChangeHarpoon Cards.seaChangeHarpoon

instance HasAbilities SeaChangeHarpoon where
  getAbilities (SeaChangeHarpoon attrs) = [restrictedAbility attrs 1 ControlsThis fightAction_]

instance HasModifiersFor SeaChangeHarpoon where
  getModifiersFor (InvestigatorTarget iid) (SeaChangeHarpoon attrs) = do
    maybeMods <- runMaybeT $ do
      source <- MaybeT getSkillTestSource
      guard $ isAbilitySource attrs 1 source
      iid' <- MaybeT getSkillTestInvestigator
      guard $ iid == iid'
      skillCount <-
        lift $ fieldMap InvestigatorCommittedCards (count (`cardMatch` CardWithType SkillType)) iid
      guard $ skillCount > 0
      pure $ DamageDealt 1

    pure $ toModifiers attrs $ maybeToList maybeMods
  getModifiersFor _ _ = pure []

instance RunMessage SeaChangeHarpoon where
  runMessage msg a@(SeaChangeHarpoon attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      pushAll
        [ skillTestModifier (attrs.ability 1) iid (SkillModifier #combat 1)
        , chooseFightEnemy iid (attrs.ability 1) #combat
        ]
      pure a
    SkillTestEnds iid (isAbilitySource attrs 1 -> True) -> do
      miid <- getSkillTestInvestigator
      when (Just iid == miid) do
        player <- getPlayer iid
        skills <- select $ skillControlledBy iid
        push
          $ chooseOne
            player
            [ Label
                "Return Sea Change Harpoon to your hand to return all of your committed skill cards to your hand instead of discarding them"
                $ ReturnToHand iid (toTarget attrs)
                : [ReturnToHand iid (toTarget skill) | skill <- skills]
            , Label "Do nothing" []
            ]
      pure a
    _ -> SeaChangeHarpoon <$> runMessage msg attrs

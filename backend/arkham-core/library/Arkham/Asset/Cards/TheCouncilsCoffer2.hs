module Arkham.Asset.Cards.TheCouncilsCoffer2 (
  theCouncilsCoffer2,
  TheCouncilsCoffer2 (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher
import Arkham.SkillType

newtype TheCouncilsCoffer2 = TheCouncilsCoffer2 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theCouncilsCoffer2 :: AssetCard TheCouncilsCoffer2
theCouncilsCoffer2 = asset TheCouncilsCoffer2 Cards.theCouncilsCoffer2

instance HasAbilities TheCouncilsCoffer2 where
  getAbilities (TheCouncilsCoffer2 a) =
    [ limitedAbility (MaxPer Cards.theCouncilsCoffer2 PerCampaign 1)
        $ restrictedAbility a 0 (if findWithDefault 0 Lock (assetUses a) == 0 then NoRestriction else Never)
        $ SilentForcedAbility AnyWindow
    , restrictedAbility a 1 OnSameLocation
        $ ActionAbility []
        $ ActionCost 2
    ]

instance RunMessage TheCouncilsCoffer2 where
  runMessage msg a@(TheCouncilsCoffer2 attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 0 _ _ -> do
      investigators <- getInvestigatorPlayers
      pushAll
        $ [ chooseOne
            player
            [ Label
                "Search Deck"
                [search iid attrs iid [(FromDeck, ShuffleBackIn)] AnyCard (PlayFoundNoCost iid 1)]
            , Label
                "Search Discard"
                [search iid attrs iid [(FromDiscard, PutBack)] AnyCard (PlayFoundNoCost iid 1)]
            ]
          | (iid, player) <- investigators
          ]
        <> [Exile (toTarget attrs)]
      pure a
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      let chooseSkillTest skillType = beginSkillTest iid (attrs.ability 1) iid skillType (Fixed 5)
      player <- getPlayer iid
      push
        $ chooseOne
          player
          [SkillLabel sType [chooseSkillTest sType] | sType <- allSkills]
      pure a
    PassedSkillTest _ _ (isAbilitySource attrs 1 -> True) SkillTestInitiatorTarget {} _ _ ->
      pure . TheCouncilsCoffer2 $ attrs & usesL . ix Lock %~ max 0 . subtract 1
    _ -> TheCouncilsCoffer2 <$> runMessage msg attrs

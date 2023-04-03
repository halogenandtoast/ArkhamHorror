module Arkham.Asset.Cards.TheCouncilsCoffer2
  ( theCouncilsCoffer2
  , TheCouncilsCoffer2(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Cost
import Arkham.Criteria
import Arkham.Matcher
import Arkham.SkillType

newtype TheCouncilsCoffer2 = TheCouncilsCoffer2 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theCouncilsCoffer2 :: AssetCard TheCouncilsCoffer2
theCouncilsCoffer2 = asset TheCouncilsCoffer2 Cards.theCouncilsCoffer2

instance HasAbilities TheCouncilsCoffer2 where
  getAbilities (TheCouncilsCoffer2 a) =
    [ limitedAbility (PerCopyLimit Cards.theCouncilsCoffer2 PerCampaign 1)
      $ restrictedAbility
          a
          0
          (if useCount (assetUses a) == 0 then NoRestriction else Never)
      $ SilentForcedAbility AnyWindow
    , restrictedAbility a 1 OnSameLocation $ ActionAbility Nothing $ ActionCost
      2
    ]

instance RunMessage TheCouncilsCoffer2 where
  runMessage msg a@(TheCouncilsCoffer2 attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 0 _ _ -> do
      iids <- getInvestigatorIds
      pushAll
        $ [ chooseOne
              iid
              [ Label
                "Search Deck"
                [ Search
                    iid
                    (toSource attrs)
                    (toTarget iid)
                    [(FromDeck, ShuffleBackIn)]
                    AnyCard
                    (PlayFoundNoCost iid 1)
                ]
              , Label
                "Search Discard"
                [ Search
                    iid
                    (toSource attrs)
                    (toTarget iid)
                    [(FromDiscard, PutBack)]
                    AnyCard
                    (PlayFoundNoCost iid 1)
                ]
              ]
          | iid <- iids
          ]
        <> [Exile (toTarget attrs)]
      pure a
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      let chooseSkillTest skillType = beginSkillTest iid attrs iid skillType 5
      push $ chooseOne
        iid
        [ SkillLabel sType [chooseSkillTest sType] | sType <- allSkills ]
      pure a
    PassedSkillTest _ _ (isSource attrs -> True) SkillTestInitiatorTarget{} _ _
      -> pure . TheCouncilsCoffer2 $ attrs & usesL %~ use
    _ -> TheCouncilsCoffer2 <$> runMessage msg attrs

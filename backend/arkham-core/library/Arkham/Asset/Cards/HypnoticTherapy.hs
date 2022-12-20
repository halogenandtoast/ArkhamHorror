module Arkham.Asset.Cards.HypnoticTherapy
  ( hypnoticTherapy
  , HypnoticTherapy(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Cost
import Arkham.Criteria
import Arkham.Damage
import Arkham.Matcher
import Arkham.SkillType
import Arkham.Target
import Arkham.Timing qualified as Timing

newtype HypnoticTherapy = HypnoticTherapy AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hypnoticTherapy :: AssetCard HypnoticTherapy
hypnoticTherapy = asset HypnoticTherapy Cards.hypnoticTherapy

instance HasAbilities HypnoticTherapy where
  getAbilities (HypnoticTherapy a) =
    [ restrictedAbility a 1 ControlsThis $ ActionAbility Nothing $ ExhaustCost
      (toTarget a)
    , restrictedAbility a 1 ControlsThis
      $ ReactionAbility
          (InvestigatorHealed Timing.After HorrorType Anyone
          $ SourceOwnedBy You
          <> NotSource (SourceIs (toSource a))
          )
      $ ExhaustCost (toTarget a)
    ]

instance RunMessage HypnoticTherapy where
  runMessage msg a@(HypnoticTherapy attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      push $ BeginSkillTest
        iid
        (toAbilitySource attrs 1)
        (InvestigatorTarget iid)
        Nothing
        SkillIntellect
        2
      pure a
    PassedSkillTest iid _ (isAbilitySource attrs 1 -> True) SkillTestInitiatorTarget{} _ _
      -> do
        targetsWithCardDraw <- do
          targets <- selectList $ colocatedWith iid <> InvestigatorWithAnyHorror
          forToSnd targets $ \i -> drawCards i (toSource attrs) 1
        when (notNull targetsWithCardDraw) $ do
          push $ chooseOrRunOne
            iid
            [ targetLabel
                target
                [ HealHorror (InvestigatorTarget target) (toSource attrs) 1
                , chooseOne
                  target
                  [ Label "Do Not Draw" []
                  , ComponentLabel
                    (InvestigatorDeckComponent target)
                    [drawing]
                  ]
                ]
            | (target, drawing) <- targetsWithCardDraw
            ]
        pure a
    UseCardAbility iid (isSource attrs -> True) 2 _ _ -> do
      pure a
    _ -> HypnoticTherapy <$> runMessage msg attrs

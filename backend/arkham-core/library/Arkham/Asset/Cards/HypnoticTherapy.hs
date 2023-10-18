module Arkham.Asset.Cards.HypnoticTherapy (
  hypnoticTherapy,
  HypnoticTherapy (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Damage
import Arkham.Helpers.Investigator
import Arkham.Matcher
import Arkham.Timing qualified as Timing
import Arkham.Window
import Data.Monoid

newtype HypnoticTherapy = HypnoticTherapy AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hypnoticTherapy :: AssetCard HypnoticTherapy
hypnoticTherapy = asset HypnoticTherapy Cards.hypnoticTherapy

instance HasAbilities HypnoticTherapy where
  getAbilities (HypnoticTherapy a) =
    [ restrictedAbility a 1 ControlsThis
        $ ActionAbility Nothing
        $ ActionCost 1
        <> exhaust a
    , restrictedAbility a 2 ControlsThis
        $ ReactionAbility
          ( InvestigatorHealed Timing.After HorrorType (affectsOthers Anyone)
              $ SourceOwnedBy You
              <> NotSource (SourceIs (toSource a))
          )
        $ exhaust a
    ]

instance RunMessage HypnoticTherapy where
  runMessage msg a@(HypnoticTherapy attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      push $ beginSkillTest iid (toAbilitySource attrs 1) iid #intellect 2
      pure a
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      targetsWithCardDrawAndHeal <- do
        iidsWithHeal <- getInvestigatorsWithHealHorror attrs 1 $ colocatedWith iid
        for iidsWithHeal $ \(i, healHorror) -> do
          draw <- drawCards i (toAbilitySource attrs 1) 1
          targetPlayer <- getPlayer i
          pure ((i, targetPlayer), draw, healHorror)
      player <- getPlayer iid
      pushWhen (notNull targetsWithCardDrawAndHeal)
        $ chooseOrRunOne player
        $ [ targetLabel target
            $ [ heal
              , chooseOne
                  targetPlayer
                  [Label "Do Not Draw" [], ComponentLabel (InvestigatorDeckComponent target) [drawing]]
              ]
          | ((target, targetPlayer), drawing, heal) <- targetsWithCardDrawAndHeal
          ]
      pure a
    UseCardAbility _ (isSource attrs -> True) 2 ws' _ -> do
      -- this is meant to heal additional so we'd directly heal one more
      -- (without triggering a window), and then overwrite the original window
      -- to heal for one more
      let
        updateHealed = \case
          Window timing (Healed HorrorType t s n) mBatchId ->
            Window timing (Healed HorrorType t s (n + 1)) mBatchId
          other -> other
        getHealedTarget = \case
          (windowType -> Healed HorrorType t _ _) -> Just t
          _ -> Nothing
        healedTarget =
          fromJustNote "wrong call"
            $ getFirst
            $ foldMap (First . getHealedTarget) ws'

      replaceMessageMatching
        \case
          RunWindow _ ws -> ws == ws'
          _ -> False
        \case
          RunWindow iid' ws -> [RunWindow iid' $ map updateHealed ws]
          _ -> error "invalid window"
      push $ HealHorrorDirectly healedTarget (toAbilitySource attrs 2) 1
      pure a
    _ -> HypnoticTherapy <$> runMessage msg attrs

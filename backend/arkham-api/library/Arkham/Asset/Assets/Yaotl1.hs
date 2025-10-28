module Arkham.Asset.Assets.Yaotl1 (yaotl1, yaotl1Effect) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Card
import Arkham.Effect.Import
import Arkham.Helpers.Modifiers
import Arkham.Helpers.SkillTest (getSkillTestInvestigator)
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Projection
import Arkham.SkillType

newtype Yaotl1 = Yaotl1 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

yaotl1 :: AssetCard Yaotl1
yaotl1 = ally Yaotl1 Cards.yaotl1 (2, 2)

instance HasAbilities Yaotl1 where
  getAbilities (Yaotl1 a) =
    [ withTooltip
        "{fast} Exhaust Yaotl: During this skill test, you get a bonus to each skill equal to the number of matching skill icons on the top card of your discard pile (not counting {skillWild} icons)."
        $ wantsSkillTest (YourSkillTest #any)
        $ controlled a 1 DuringAnySkillTest
        $ FastAbility
        $ exhaust a
    , withTooltip "{fast}: Discard the top card of your deck. (Limit once per phase.)"
        $ playerLimit PerPhase
        $ controlled a 2 CanManipulateDeck
        $ FastAbility Free
    ]

instance RunMessage Yaotl1 where
  runMessage msg a@(Yaotl1 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      createCardEffect Cards.yaotl1 Nothing (attrs.ability 1) iid
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      discardTopOfDeck iid (attrs.ability 2) 1
      pure a
    _ -> Yaotl1 <$> liftRunMessage msg attrs

newtype Yaotl1Effect = Yaotl1Effect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

yaotl1Effect :: EffectArgs -> Yaotl1Effect
yaotl1Effect = cardEffect Yaotl1Effect Cards.yaotl1

instance HasModifiersFor Yaotl1Effect where
  getModifiersFor (Yaotl1Effect a) = case a.target of
    InvestigatorTarget iid -> maybeModified_ a iid do
      iid' <- MaybeT getSkillTestInvestigator
      guard $ iid == iid'
      x :| _ <- MaybeT $ fieldMap InvestigatorDiscard nonEmpty iid
      let
        skillIcons = cdSkills $ toCardDef x
        skillCount sk = count (== SkillIcon sk) skillIcons
      pure [SkillModifier sk n | sk <- allSkills, let n = skillCount sk, n > 0]
    _ -> pure mempty

instance RunMessage Yaotl1Effect where
  runMessage msg e@(Yaotl1Effect attrs) = runQueueT $ case msg of
    SkillTestEnds {} -> disableReturn e
    _ -> Yaotl1Effect <$> liftRunMessage msg attrs

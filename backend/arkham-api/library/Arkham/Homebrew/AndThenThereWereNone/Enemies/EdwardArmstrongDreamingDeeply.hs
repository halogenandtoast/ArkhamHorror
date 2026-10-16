{- | The Doctor, unmasked. Each enemy phase he bleeds the party's hands by the
offerings already laid down, then lays down four more.
-}
module Arkham.Homebrew.AndThenThereWereNone.Enemies.EdwardArmstrongDreamingDeeply (
  edwardArmstrongDreamingDeeply,
) where

import Arkham.Ability
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Message.Discard.Lifted (chooseAndDiscardCard)
import Arkham.Homebrew.AndThenThereWereNone.CardDefs.Enemies qualified as Cards
import Arkham.Homebrew.AndThenThereWereNone.Helpers
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype EdwardArmstrongDreamingDeeply = EdwardArmstrongDreamingDeeply EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

edwardArmstrongDreamingDeeply :: EnemyCard EdwardArmstrongDreamingDeeply
edwardArmstrongDreamingDeeply =
  enemy EdwardArmstrongDreamingDeeply Cards.edwardArmstrongDreamingDeeply

instance HasAbilities EdwardArmstrongDreamingDeeply where
  getAbilities (EdwardArmstrongDreamingDeeply a) =
    extend1 a
      $ scenarioI18n
      $ withI18nTooltip "edwardArmstrongDreamingDeeply.dreaming"
      $ mkAbility a 1
      $ forced
      $ PhaseEnds #when #enemy

instance RunMessage EdwardArmstrongDreamingDeeply where
  runMessage msg e@(EdwardArmstrongDreamingDeeply attrs) = runQueueT $ case msg of
    -- "the investigators as a group must choose and discard X cards from their
    -- hands, where X is the total amount of offerings among all copies of
    -- Antediluvian Sigils. Then, add 1 offering to each copy."
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      n <- totalOfferings
      repeated n $ doStep 1 msg
      selectEach antediluvianSigils $ placeTokensOn (attrs.ability 1) #offering 1
      pure e
    -- One card of the group's total, so the table re-decides who pays for each.
    DoStep 1 (UseThisAbility _ (isSource attrs -> True) 1) -> do
      iids <- select $ HandWith (HasCard DiscardableCard)
      leadChooseOrRunOneM $ targets iids \iid -> chooseAndDiscardCard iid (attrs.ability 1)
      pure e
    _ -> EdwardArmstrongDreamingDeeply <$> liftRunMessage msg attrs

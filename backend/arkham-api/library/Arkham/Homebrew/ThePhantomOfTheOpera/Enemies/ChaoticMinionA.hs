module Arkham.Homebrew.ThePhantomOfTheOpera.Enemies.ChaoticMinionA (chaoticMinionA) where

import Arkham.Ability
import Arkham.Campaigns.TheScarletKeys.Concealed.Helpers (resolveConcealed)
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelectMap)
import Arkham.Homebrew.ThePhantomOfTheOpera.CardDefs.Enemies qualified as Cards
import Arkham.Homebrew.ThePhantomOfTheOpera.Kinds qualified as Kinds
import Arkham.Keyword qualified as Keyword
import Arkham.Matcher
import Arkham.Message.Lifted.Placement

newtype ChaoticMinionA = ChaoticMinionA EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

chaoticMinionA :: EnemyCard ChaoticMinionA
chaoticMinionA = enemy ChaoticMinionA Cards.chaoticMinionA

instance HasModifiersFor ChaoticMinionA where
  getModifiersFor (ChaoticMinionA a) = when (a.placement == InTheShadows) do
    modifySelectMap a Anyone \iid ->
      [IncreaseCostOf (inHandOf ForPlay iid <> basic (#event <> NonWeakness)) 1]

instance HasAbilities ChaoticMinionA where
  getAbilities (ChaoticMinionA a) =
    extend1 a
      $ restricted a 1 (thisExists a $ EnemyAt Anywhere)
      $ forced
      $ PhaseEnds #when #enemy

instance RunMessage ChaoticMinionA where
  runMessage msg e@(ChaoticMinionA attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      roundModifier (attrs.ability 1) attrs
        $ AddKeyword (Keyword.Concealed Kinds.ChaoticMinionA (Static 1))
      doStep 1 msg
      pure e
    DoStep 1 (UseThisAbility iid (isSource attrs -> True) 1) -> do
      place attrs InTheShadows
      resolveConcealed iid attrs.id
      pure e
    _ -> ChaoticMinionA <$> liftRunMessage msg attrs

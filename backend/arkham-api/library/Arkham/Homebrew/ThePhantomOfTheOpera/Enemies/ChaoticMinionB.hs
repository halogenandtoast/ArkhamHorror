module Arkham.Homebrew.ThePhantomOfTheOpera.Enemies.ChaoticMinionB (chaoticMinionB) where

import Arkham.Ability
import Arkham.Campaigns.TheScarletKeys.Concealed.Helpers (resolveConcealed)
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelectMap)
import Arkham.Homebrew.ThePhantomOfTheOpera.CardDefs.Enemies qualified as Cards
import Arkham.Homebrew.ThePhantomOfTheOpera.Kinds qualified as Kinds
import Arkham.Keyword qualified as Keyword
import Arkham.Matcher
import Arkham.Message.Lifted.Placement

newtype ChaoticMinionB = ChaoticMinionB EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

chaoticMinionB :: EnemyCard ChaoticMinionB
chaoticMinionB = enemy ChaoticMinionB Cards.chaoticMinionB

instance HasModifiersFor ChaoticMinionB where
  getModifiersFor (ChaoticMinionB a) = when (a.placement == InTheShadows) do
    modifySelectMap a Anyone \iid ->
      [IncreaseCostOf (inHandOf ForPlay iid <> basic (#asset <> NonWeakness)) 1]

instance HasAbilities ChaoticMinionB where
  getAbilities (ChaoticMinionB a) =
    extend1 a
      $ restricted a 1 (thisExists a $ EnemyAt Anywhere)
      $ forced
      $ PhaseEnds #when #enemy

instance RunMessage ChaoticMinionB where
  runMessage msg e@(ChaoticMinionB attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      roundModifier (attrs.ability 1) attrs
        $ AddKeyword (Keyword.Concealed Kinds.ChaoticMinionB (Static 1))
      doStep 1 msg
      pure e
    DoStep 1 (UseThisAbility iid (isSource attrs -> True) 1) -> do
      place attrs InTheShadows
      resolveConcealed iid attrs.id
      pure e
    _ -> ChaoticMinionB <$> liftRunMessage msg attrs

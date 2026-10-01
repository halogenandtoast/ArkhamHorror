module Arkham.Homebrew.CircusExMortis.Enemies.DevoteeOfTheThousand (devoteeOfTheThousand) where

import Arkham.Ability
import Arkham.Enemy.Import.Lifted
import Arkham.GameEnv (getCard)
import Arkham.Homebrew.CircusExMortis.CardDefs.Enemies qualified as Cards
import Arkham.Matcher
import Arkham.Message qualified as Msg

newtype DevoteeOfTheThousand = DevoteeOfTheThousand EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

devoteeOfTheThousand :: EnemyCard DevoteeOfTheThousand
devoteeOfTheThousand = enemy DevoteeOfTheThousand Cards.devoteeOfTheThousand

instance HasAbilities DevoteeOfTheThousand where
  getAbilities (DevoteeOfTheThousand a) =
    extend1 a $ restricted a 1 (thisExists a AnyEnemy) $ forced $ EnemyLeavesPlay #when (be a)

instance RunMessage DevoteeOfTheThousand where
  runMessage msg e@(DevoteeOfTheThousand attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      -- It still leaves play, so the removal itself stands; only its destination
      -- is replaced. Dropping the queued discard is what keeps the card out of
      -- the encounter discard so `SetCardAside` is the only place it lands.
      card <- getCard attrs.cardId
      allMatchingDon't \case
        Msg.Discarded target _ _ -> isTarget attrs target
        Do (Msg.Discarded target _ _) -> isTarget attrs target
        _ -> False
      push $ SetCardAside card
      pure e
    _ -> DevoteeOfTheThousand <$> liftRunMessage msg attrs

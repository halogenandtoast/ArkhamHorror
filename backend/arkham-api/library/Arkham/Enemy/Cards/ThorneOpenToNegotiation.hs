module Arkham.Enemy.Cards.ThorneOpenToNegotiation (thorneOpenToNegotiation) where

import Arkham.Ability
import Arkham.Campaigns.TheScarletKeys.Helpers
import Arkham.Campaigns.TheScarletKeys.Key.Matcher
import Arkham.Campaigns.TheScarletKeys.Key.Types (Field (..))
import Arkham.Campaigns.TheScarletKeys.Meta
import Arkham.Card.CardDef
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted hiding (keys)
import Arkham.Helpers.Query (getLead)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Placement
import Arkham.Projection

newtype ThorneOpenToNegotiation = ThorneOpenToNegotiation EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

thorneOpenToNegotiation :: EnemyCard ThorneOpenToNegotiation
thorneOpenToNegotiation = enemy ThorneOpenToNegotiation Cards.thorneOpenToNegotiation (4, Static 4, 4) (1, 1)

instance HasAbilities ThorneOpenToNegotiation where
  getAbilities (ThorneOpenToNegotiation a) =
    extend
      a
      [ mkAbility a 1 $ forced $ EnemyEntersPlay #after (be a <> EnemyWithAnyScarletKey)
      , restricted a 2 (youExist InvestigatorWithAnyScarletKey)
          $ FastAbility Free
      ]

instance RunMessage ThorneOpenToNegotiation where
  runMessage msg e@(ThorneOpenToNegotiation attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      skeys <- select $ ScarletKeyWithPlacement (AttachedToEnemy attrs.id)
      lead <- getLead
      chooseOneAtATimeM lead $ targets skeys shift
      pure e
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      keys <- select $ ScarletKeyWithPlacement (AttachedToInvestigator iid)
      chooseTargetM iid keys \key -> do
        push $ RemoveScarletKey key
        card <- field ScarletKeyCard key
        addToVictory iid attrs
        setBearer (toCardDef card) (keyWithEnemy Cards.thorneOpenToNegotiation)
      pure e
    _ -> ThorneOpenToNegotiation <$> liftRunMessage msg attrs

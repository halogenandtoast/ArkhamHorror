module Arkham.Enemy.Cards.MimeticNemesisInfiltratorOfRealities (mimeticNemesisInfiltratorOfRealities) where

import Arkham.Ability
import Arkham.Campaigns.TheScarletKeys.Concealed
import Arkham.Campaigns.TheScarletKeys.Concealed.Helpers
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Enemy
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype MimeticNemesisInfiltratorOfRealities = MimeticNemesisInfiltratorOfRealities EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mimeticNemesisInfiltratorOfRealities :: EnemyCard MimeticNemesisInfiltratorOfRealities
mimeticNemesisInfiltratorOfRealities =
  enemy
    MimeticNemesisInfiltratorOfRealities
    Cards.mimeticNemesisInfiltratorOfRealities
    (5, Static 3, 5)
    (3, 3)

instance HasModifiersFor MimeticNemesisInfiltratorOfRealities where
  getModifiersFor (MimeticNemesisInfiltratorOfRealities a) = do
    n <- perPlayer 3
    modifySelf a [HealthModifier n, CannotMakeAttacksOfOpportunity]

instance HasAbilities MimeticNemesisInfiltratorOfRealities where
  getAbilities (MimeticNemesisInfiltratorOfRealities a) =
    extend1 a
      $ mkAbility a 1
      $ forced
      $ EnemyWouldTakeDamage #when (SourceOwnedBy You) (be a)

instance RunMessage MimeticNemesisInfiltratorOfRealities where
  runMessage msg e@(MimeticNemesisInfiltratorOfRealities attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      concealedCards <- select $ not_ ExposedConcealedCard
      chooseOneM iid do
        for_ concealedCards \card ->
          targeting card.id do
            turnOverConcealed iid (attrs.ability 1) card.id
            if card.kind == MimeticNemesis
              then do
                chooseTargetM iid [card] (const nothing)
                scenarioSpecific_ "shuffleAllConcealed"
              else do_ msg
      pure e
    Do (UseThisAbility _iid (isSource attrs -> True) 1) -> do
      insteadOfDamage attrs \_ -> pure ()
      pure e
    _ -> MimeticNemesisInfiltratorOfRealities <$> liftRunMessage msg attrs

module Arkham.Treachery.Cards.MyriadForms (myriadForms, MyriadForms (..)) where

import Arkham.Attack
import Arkham.Card
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Enemy.Types (Field (..))
import Arkham.Matcher
import Arkham.Name
import Arkham.Projection
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype MyriadForms = MyriadForms TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

myriadForms :: TreacheryCard MyriadForms
myriadForms = treacheryWith MyriadForms Cards.myriadForms (setMeta @Bool False)

instance RunMessage MyriadForms where
  runMessage msg t@(MyriadForms attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      inHand <-
        selectWithField EnemyCard $ EnemyInHandOf (InvestigatorWithId iid) <> EnemyWithTitle "Nyarlathotep"
      mInPlay <- selectOne $ enemyIs Enemies.nyarlathotepTrueShape

      for_ inHand $ \(_, card) -> push $ RevealCard (toCardId card)
      for_ inHand $ \(nyarlathotep, _) -> push $ EnemyWillAttack $ enemyAttack nyarlathotep (toSource attrs) iid
      for_ inHand $ \(nyarlathotep, _) -> push $ ShuffleBackIntoEncounterDeck (toTarget nyarlathotep)

      for_ mInPlay $ \nyarlathotep -> do
        pushAll
          [SendMessage (toTarget nyarlathotep) HuntersMove, SendMessage (toTarget nyarlathotep) EnemiesAttack]

      push $ DoStep 1 msg

      pure t
    PerformEnemyAttack details -> do
      isNyarlathotep <- fieldMap EnemyName (`hasTitle` "Nyarlathotep") (attackEnemy details)
      let meta = toResult @Bool attrs.meta
      pure $ MyriadForms $ setMeta @Bool (meta || isNyarlathotep) attrs
    DoStep 1 (Revelation iid (isSource attrs -> True)) -> do
      unless (toResult @Bool attrs.meta) $ gainSurge attrs
      pure t
    _ -> MyriadForms <$> lift (runMessage msg attrs)

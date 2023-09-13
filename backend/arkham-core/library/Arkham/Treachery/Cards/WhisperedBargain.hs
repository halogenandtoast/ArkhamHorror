module Arkham.Treachery.Cards.WhisperedBargain (
  whisperedBargain,
  WhisperedBargain (..),
)
where

import Arkham.Prelude

import Arkham.Attack
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Matcher
import Arkham.Message
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype WhisperedBargain = WhisperedBargain TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

whisperedBargain :: TreacheryCard WhisperedBargain
whisperedBargain = treachery WhisperedBargain Cards.whisperedBargain

instance RunMessage WhisperedBargain where
  runMessage msg t@(WhisperedBargain attrs) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      azathoth <- selectJust $ IncludeOmnipotent $ enemyIs Enemies.azathoth
      push
        $ chooseOne
          iid
          [ Label "Place 1 Doom on Azathoth" [PlaceDoom (toSource attrs) (toTarget azathoth) 1]
          , Label
              "Azathoth attacks each you"
              [toMessage $ enemyAttack azathoth (toSource attrs) iid]
          ]
      pure t
    _ -> WhisperedBargain <$> runMessage msg attrs

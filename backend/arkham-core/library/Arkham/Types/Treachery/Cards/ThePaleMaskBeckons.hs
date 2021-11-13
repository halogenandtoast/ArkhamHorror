module Arkham.Types.Treachery.Cards.ThePaleMaskBeckons
  ( thePaleMaskBeckons
  , ThePaleMaskBeckons(..)
  ) where

import Arkham.Prelude

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Helpers
import Arkham.Types.Treachery.Runner

newtype ThePaleMaskBeckons = ThePaleMaskBeckons TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor env, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

thePaleMaskBeckons :: TreacheryCard ThePaleMaskBeckons
thePaleMaskBeckons = treachery ThePaleMaskBeckons Cards.thePaleMaskBeckons

instance TreacheryRunner env => RunMessage env ThePaleMaskBeckons where
  runMessage msg t@(ThePaleMaskBeckons attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      mTheManInThePallidMask <- selectOne $ enemyIs Cards.theManInThePallidMask
      t <$ case mTheManInThePallidMask of
        Just enemy -> do
          iids <- getInvestigatorIds
          pushAll $ map (\i -> EnemyAttack i enemy DamageAny) iids
        Nothing -> do
          enemy <- getCampaignStoryCard Cards.theManInThePallidMask ()
          pushAll
            [ RemoveFromBearersDeckOrDiscard enemy
            , DrewPlayerEnemy iid (PlayerCard enemy)
            ]
    _ -> ThePaleMaskBeckons <$> runMessage msg attrs

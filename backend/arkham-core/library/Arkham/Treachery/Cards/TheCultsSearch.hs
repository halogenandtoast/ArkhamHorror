module Arkham.Treachery.Cards.TheCultsSearch
  ( theCultsSearch
  , TheCultsSearch(..)
  ) where

import Arkham.Prelude

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Card.CardType
import Arkham.Classes
import Arkham.Matcher
import Arkham.Message
import Arkham.Query
import Arkham.Target
import Arkham.Trait
import Arkham.Treachery.Attrs
import Arkham.Treachery.Runner

newtype TheCultsSearch = TheCultsSearch TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor env, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theCultsSearch :: TreacheryCard TheCultsSearch
theCultsSearch = treachery TheCultsSearch Cards.theCultsSearch

instance TreacheryRunner env => RunMessage TheCultsSearch where
  runMessage msg t@(TheCultsSearch attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      cultists <- selectList $ EnemyWithTrait Cultist <> EnemyWithAnyDoom
      cultistsWithDoomCount <- traverse
        (traverseToSnd (fmap unDoomCount . getCount))
        cultists
      let
        revelation = if null cultistsWithDoomCount
          then
            [ FindAndDrawEncounterCard
                iid
                (CardWithType EnemyType <> CardWithTrait Cultist)
            ]
          else
            concatMap
                (\(enemy, doom) ->
                  RemoveDoom (EnemyTarget enemy) doom
                    : replicate doom PlaceDoomOnAgenda
                )
                cultistsWithDoomCount
              <> [AdvanceAgendaIfThresholdSatisfied]
      t <$ pushAll revelation
    _ -> TheCultsSearch <$> runMessage msg attrs

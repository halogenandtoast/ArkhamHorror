module Arkham.Types.Treachery.Cards.TheCultsSearch
  ( theCultsSearch
  , TheCultsSearch(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Treachery.Cards as Cards
import Arkham.Types.Card.CardType
import Arkham.Types.Classes
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Query
import Arkham.Types.Target
import Arkham.Types.Trait
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner

newtype TheCultsSearch = TheCultsSearch TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor env, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theCultsSearch :: TreacheryCard TheCultsSearch
theCultsSearch = treachery TheCultsSearch Cards.theCultsSearch

instance TreacheryRunner env => RunMessage env TheCultsSearch where
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
      t <$ pushAll (revelation <> [Discard $ toTarget attrs])
    _ -> TheCultsSearch <$> runMessage msg attrs

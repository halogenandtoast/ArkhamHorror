module Arkham.Treachery.Cards.TheCultsSearch (
  theCultsSearch,
  TheCultsSearch (..),
) where

import Arkham.Prelude

import Arkham.Card.CardType
import Arkham.Classes
import Arkham.Enemy.Types (Field (..))
import Arkham.Matcher
import Arkham.Message
import Arkham.Trait
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype TheCultsSearch = TheCultsSearch TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theCultsSearch :: TreacheryCard TheCultsSearch
theCultsSearch = treachery TheCultsSearch Cards.theCultsSearch

instance RunMessage TheCultsSearch where
  runMessage msg t@(TheCultsSearch attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      cultists <-
        selectWithField EnemyDoom $ EnemyWithTrait Cultist <> EnemyWithAnyDoom
      let
        revelation =
          if null cultists
            then
              [ findAndDrawEncounterCard
                  iid
                  (CardWithType EnemyType <> CardWithTrait Cultist)
              ]
            else
              concatMap
                ( \(enemy, doom) ->
                    RemoveDoom (toSource attrs) (toTarget enemy) doom
                      : replicate doom PlaceDoomOnAgenda
                )
                cultists
                <> [AdvanceAgendaIfThresholdSatisfied]
      pushAll revelation
      pure t
    _ -> TheCultsSearch <$> runMessage msg attrs

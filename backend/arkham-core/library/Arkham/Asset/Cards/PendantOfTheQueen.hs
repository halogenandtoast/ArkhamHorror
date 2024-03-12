module Arkham.Asset.Cards.PendantOfTheQueen (
  pendantOfTheQueen,
  PendantOfTheQueen (..),
)
where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Card
import Arkham.Deck qualified as Deck
import Arkham.Helpers.Investigator (searchBonded)
import Arkham.Matcher hiding (EnemyEvaded)
import Arkham.Movement

newtype PendantOfTheQueen = PendantOfTheQueen AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

pendantOfTheQueen :: AssetCard PendantOfTheQueen
pendantOfTheQueen = assetWith PendantOfTheQueen Cards.pendantOfTheQueen $ whenNoUsesL ?~ NotifySelfOfNoUses

{- Exhaust Pendant of the Queen and spend 1 charge: Choose a revealed location and select one - move to that location, discover 1 clue at that location, or automatically evade an enemy at that location. -}

instance HasAbilities PendantOfTheQueen where
  getAbilities (PendantOfTheQueen attrs) =
    [ controlledAbility
        attrs
        1
        ( AnyCriterion
            [ exists (You <> InvestigatorCanMove) <> exists (RevealedLocation <> Unblocked <> NotYourLocation)
            , exists
                ( RevealedLocation
                    <> oneOf
                      [ LocationWithDiscoverableCluesBy You <> LocationWithAnyClues
                      , LocationWithEnemy (EnemyWithEvade <> EnemyWithoutModifier CannotBeEvaded)
                      ]
                )
            ]
        )
        $ FastAbility
        $ exhaust attrs
        <> assetUseCost attrs Charge 1
    ]

instance RunMessage PendantOfTheQueen where
  runMessage msg a@(PendantOfTheQueen attrs) = case msg of
    SpentAllUses (isTarget attrs -> True) -> do
      for_ attrs.controller $ \controller -> do
        segments <- take 3 <$> searchBonded controller Cards.segmentOfOnyx1
        pushAll
          [ PlaceInBonded controller (toCard attrs)
          , ShuffleCardsIntoDeck (Deck.InvestigatorDeck controller) segments
          ]
      pure a
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      canMove <- iid <=~> InvestigatorCanMove
      locations <-
        select
          $ RevealedLocation
          <> oneOf
            ( (LocationWithDiscoverableCluesBy (InvestigatorWithId iid) <> LocationWithAnyClues)
                : LocationWithEnemy (EnemyWithEvade <> EnemyWithoutModifier CannotBeEvaded)
                : [NotLocation (locationWithInvestigator iid) <> Unblocked | canMove]
            )
      player <- getPlayer iid
      push
        $ chooseOrRunOne player
        $ targetLabels locations (only . HandleTargetChoice iid (toAbilitySource attrs 1) . toTarget)

      pure a
    HandleTargetChoice iid (isAbilitySource attrs 1 -> True) (LocationTarget lid) -> do
      canMove <- iid <=~> InvestigatorCanMove
      moveChoice <- lid <=~> (NotLocation (locationWithInvestigator iid) <> Unblocked)
      discoverChoice <-
        lid <=~> (LocationWithDiscoverableCluesBy (InvestigatorWithId iid) <> LocationWithAnyClues)
      enemies <-
        select $ EnemyAt (LocationWithId lid) <> EnemyWithEvade <> EnemyWithoutModifier CannotBeEvaded
      player <- getPlayer iid

      push
        $ chooseOrRunOne player
        $ [ Label "Move to this location" [MoveTo $ move (toAbilitySource attrs 1) iid lid]
          | canMove && moveChoice
          ]
        <> [ Label
            "Discover a clue at this location"
            [DiscoverCluesAtLocation iid lid (toAbilitySource attrs 1) 1 NotInvestigate Nothing]
           | discoverChoice
           ]
        <> [ Label
            "Evade an enemy at this location"
            [chooseOrRunOne player $ targetLabels enemies (only . EnemyEvaded iid)]
           | notNull enemies
           ]
      pure a
    _ -> PendantOfTheQueen <$> runMessage msg attrs

module Arkham.Asset.Assets.PendantOfTheQueen (pendantOfTheQueen) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Helpers.Investigator (searchBonded)
import Arkham.Helpers.Modifiers
import Arkham.Matcher hiding (EnemyEvaded)
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move
import Arkham.Taboo

newtype PendantOfTheQueen = PendantOfTheQueen AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

pendantOfTheQueen :: AssetCard PendantOfTheQueen
pendantOfTheQueen = assetWith PendantOfTheQueen Cards.pendantOfTheQueen $ whenNoUsesL ?~ NotifySelfOfNoUses

{- Exhaust Pendant of the Queen and spend 1 charge: Choose a revealed location and select one - move to that location, discover 1 clue at that location, or automatically evade an enemy at that location. -}

instance HasAbilities PendantOfTheQueen where
  getAbilities (PendantOfTheQueen a) =
    [ controlled
        a
        1
        ( oneOf
            [ youExist InvestigatorCanMove <> exists (RevealedLocation <> Unblocked <> NotYourLocation)
            , exists
                $ RevealedLocation
                <> oneOf
                  [ LocationWithDiscoverableCluesBy You
                  , LocationWithEnemy (EnemyWithEvade <> EnemyWithoutModifier CannotBeEvaded)
                  ]
            ]
        )
        $ FastAbility (exhaust a <> assetUseCost a Charge 1)
    ]

instance RunMessage PendantOfTheQueen where
  runMessage msg a@(PendantOfTheQueen attrs) = runQueueT $ case msg of
    SpentAllUses (isTarget attrs -> True) -> do
      if tabooed TabooList19 attrs
        then removeFromGame attrs
        else for_ attrs.controller $ \controller -> do
          segments <- take 3 <$> searchBonded controller Cards.segmentOfOnyx1
          placeInBonded controller attrs
          shuffleCardsIntoDeck controller segments
      pure a
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      canMove <- iid <=~> InvestigatorCanMove
      locations <-
        select
          $ RevealedLocation
          <> oneOf
            ( LocationWithDiscoverableCluesBy (InvestigatorWithId iid)
                : LocationWithEnemy (EnemyWithEvade <> EnemyWithoutModifier CannotBeEvaded)
                : [not_ (locationWithInvestigator iid) <> Unblocked | canMove]
            )
      chooseOrRunOneM iid do
        targets locations (handleTarget iid (attrs.ability 1))

      pure a
    HandleTargetChoice iid (isAbilitySource attrs 1 -> True) (LocationTarget lid) -> do
      canMove <- iid <=~> InvestigatorCanMove
      moveChoice <- lid <=~> (NotLocation (locationWithInvestigator iid) <> Unblocked)
      discoverChoice <- lid <=~> LocationWithDiscoverableCluesBy (InvestigatorWithId iid)
      enemies <-
        select $ at_ (LocationWithId lid) <> EnemyWithEvade <> EnemyWithoutModifier CannotBeEvaded

      chooseOrRunOneM iid do
        when (canMove && moveChoice) do
          labeled "Move to this location" $ moveTo (attrs.ability 1) iid lid

        when discoverChoice do
          labeled "Discover a clue at this location" $ discoverAt NotInvestigate iid (attrs.ability 1) 1 lid

        when (notNull enemies) do
          labeled "Evade an enemy at this location" do
            chooseOrRunOneM iid $ targets enemies (automaticallyEvadeEnemy iid)
      pure a
    _ -> PendantOfTheQueen <$> liftRunMessage msg attrs

module Arkham.Homebrew.DarkMatter.Acts.Psychoanalysis (psychoanalysis) where

import Arkham.Ability
import Arkham.Act.Import.Lifted
import Arkham.Homebrew.DarkMatter.CardDefs.Acts qualified as Cards
import Arkham.Homebrew.DarkMatter.CardDefs.Assets qualified as Assets
import Arkham.Homebrew.DarkMatter.CardDefs.Enemies qualified as Enemies
import Arkham.Homebrew.DarkMatter.CardDefs.Locations qualified as Locations
import Arkham.Homebrew.DarkMatter.Traits (pattern School)
import Arkham.Location.Grid
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Placement

newtype Psychoanalysis = Psychoanalysis ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

psychoanalysis :: ActCard Psychoanalysis
psychoanalysis = act (2, A) Psychoanalysis Cards.psychoanalysis Nothing

instance HasAbilities Psychoanalysis where
  getAbilities (Psychoanalysis a) =
    [ mkAbility a 1 $ FastAbility (GroupClueCost (PerPlayer 1) Anywhere)
    , restricted
        a
        2
        ( exists (LocationInPosition (Pos (-1) 2) <> locationIs Locations.classroomK2)
            <> exists (LocationInPosition (Pos 0 2) <> locationIs Locations.cafeteria)
            <> exists (LocationInPosition (Pos 1 2) <> locationIs Locations.gymnasium)
            <> exists (LocationInPosition (Pos (-1) 1) <> locationIs Locations.library)
            <> exists (LocationInPosition (Pos 0 1) <> locationIs Locations.entranceHall)
            <> exists (LocationInPosition (Pos 1 1) <> locationIs Locations.biologyLab)
        )
        $ Objective
        $ forced
        $ RoundEnds #when
    ]

instance RunMessage Psychoanalysis where
  runMessage msg a@(Psychoanalysis attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      locations <- select $ LocationWithTrait School
      chooseOneM iid $ targets locations \first' -> do
        adjacent <- select $ connectedFrom (LocationWithId first') <> LocationWithTrait School
        chooseOneM iid $ targets adjacent \second' ->
          push $ ScenarioSpecific "switchLocations" (toJSON (first', second'))
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      advanceVia #other attrs attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      whenM (selectNone $ enemyIs Enemies.theBOOGEYMAN) do
        entranceHall <- selectJust $ locationIs Locations.entranceHall
        createSetAsideEnemy_ Enemies.theBOOGEYMAN entranceHall

      revealMatching UnrevealedLocation
      doStep 1 msg
      advanceActDeck attrs
      pure a
    DoStep 1 (AdvanceAct (isSide B attrs -> True) _ _) -> do
      boogeymanLocation <- selectOne $ LocationWithEnemy (enemyIs Enemies.theBOOGEYMAN)
      cafeteria <- selectJust $ locationIs Locations.cafeteria
      avatars <- shuffle =<< fetchCards [Assets.alma, Assets.david, Assets.tilde, Assets.william]
      locations <-
        select
          $ mapOneOf
            locationIs
            [Locations.classroomK2, Locations.library, Locations.gymnasium, Locations.biologyLab]
      for_ (zip avatars locations) \(avatar, lid) ->
        createAssetAt_ avatar
          $ AttachedToLocation (if Just lid == boogeymanLocation then cafeteria else lid)

      for_ boogeymanLocation \loc ->
        selectEach
          (assetIs Assets.maja <> at_ (be loc <> not_ (be cafeteria)))
          (`place` AtLocation cafeteria)

      pure a
    _ -> Psychoanalysis <$> liftRunMessage msg attrs

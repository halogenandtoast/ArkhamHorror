module Arkham.Homebrew.AgesUnwound.Acts.GettingYourBearings (gettingYourBearings) where

import Arkham.Ability
import Arkham.Act.Import.Lifted
import Arkham.Card
import Arkham.Deck qualified as Deck
import Arkham.Enemy.Types (Field (EnemyPlacement))
import Arkham.ForMovement
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Helpers.Modifiers (ModifierType (..))
import Arkham.Homebrew.AgesUnwound.CardDefs.Acts qualified as Cards
import Arkham.Homebrew.AgesUnwound.CardDefs.Enemies qualified as Enemies
import Arkham.Homebrew.AgesUnwound.CardDefs.Locations qualified as Locations
import Arkham.Homebrew.AgesUnwound.ScenarioDeckKeys (pattern ArkhamStreetsDeck)
import Arkham.Homebrew.AgesUnwound.Scenarios.NightOfFire.Helpers
import Arkham.Keyword qualified as Keyword
import Arkham.Location.Types qualified as Field
import Arkham.Matcher
import Arkham.Message (ReplaceStrategy (Swap))
import Arkham.Placement
import Arkham.Projection
import Arkham.Token qualified as Token
import Arkham.Trait (Trait (Arkham))

newtype GettingYourBearings = GettingYourBearings ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

gettingYourBearings :: ActCard GettingYourBearings
gettingYourBearings = act (2, A) GettingYourBearings Cards.gettingYourBearings Nothing

{- | "a connecting location" -- and not a street that is itself being dealt.

A new Arkham Streets location is put into play facedown and is only revealed
when someone arrives, so an /unrevealed/ destination is already "a new Arkham
Streets location" and must not be offered this redirect: otherwise every card
that says "move to a new Arkham Streets location" would re-enter this window and
could keep dealing the deck down. Twisting Alleys reads its own condition the
same way.
-}
connectingDestination :: LocationMatcher
connectingDestination = ConnectedLocation ForMovement <> RevealedLocation

{- | Unlike On the Lam, this act's reaction prints no "you may initiate a move"
parenthetical, so it gets no move-permission ability: by the time it is in play
the streets have started to stay on the table, and its own Forced only sweeps
away locations that are both empty /and/ resourceless.
-}
instance HasAbilities GettingYourBearings where
  getAbilities (GettingYourBearings a) =
    [ mkAbility a 1
        $ triggered (WouldMove #when You #any Anywhere connectingDestination) Free
    , mkAbility a 2 $ forced $ RoundEnds #when
    , restricted a 3 (exists $ You <> at_ Anywhere)
        $ FastAbility (GroupClueCost (PerPlayer 1) YourLocation)
    , restricted
        a
        4
        ( LocationCount 8 (LocationWithTrait Arkham)
            <> notExists (Anywhere <> LocationWithResources (atMost 0))
        )
        $ Objective
        $ forced AnyWindow
    ]

instance RunMessage GettingYourBearings where
  runMessage msg a@(GettingYourBearings attrs) = runQueueT $ case msg of
    -- "[reaction] When an effect would allow you to move to a connecting
    -- location: Instead, move to a new Arkham Streets location."
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      redirectToNewArkhamStreetsLocation (attrs.ability 1) iid
      pure a
    -- "Forced - At the end of the round: Shuffle each empty location without a
    -- resource on into the Arkham Streets deck."
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      selectEach (EmptyLocation <> LocationWithResources (atMost 0)) \lid -> do
        card <- field Field.LocationCard lid
        removeLocation lid
        shuffleCardsIntoDeck (Deck.ScenarioDeckByKey ArkhamStreetsDeck) [card]
      pure a
    {- "[free] Investigators at your location spend 1[per_investigator] clues, as
    a group: Place a resource on your location (from the token pool)." -}
    UseThisAbility iid (isSource attrs -> True) 3 -> do
      selectForMaybeM (locationWithInvestigator iid) \lid ->
        placeTokens (attrs.ability 3) lid Token.Resource 1
      pure a
    UseThisAbility _ (isSource attrs -> True) 4 -> do
      advanceVia #other attrs attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      -- "Place 1[per_investigator] clues on each of A Place to Hide, Winchmore
      -- House and Cramped Passage."
      n <- perPlayer 1
      selectEach
        ( mapOneOf
            locationIs
            [Locations.aPlaceToHide, Locations.winchmoreHouse, Locations.crampedPassage]
        )
        \lid -> placeTokens attrs lid Token.Clue n

      -- "Search the encounter deck and discard pile for a copy of Hired Thugs
      -- and spawn them at Winchmore House."
      lead <- getLead
      findEncounterCard lead attrs (cardIs Enemies.hiredThugs)

      {- "If Eternity's Sentinel (Scourge in the Shadows) is in play, replace it
      with the set-aside Eternity's Sentinel (Watcher of the Ages) (it keeps all
      tokens and attachments). Then, if it is in play but at no location, spawn
      it at Rivertown."

      'Swap' is the right strategy and does the parenthetical for free: it keeps
      the enemy id and copies the tokens, placement, attachments and exhausted
      state over to the new card. Because the id survives, the "at no location"
      check below reads the same enemy either way. -}
      selectEach
        (mapOneOf enemyIs [Enemies.eternitysSentinel_016, Enemies.eternitysSentinel_017])
        \sentinel -> do
          isScourge <- sentinel <=~> enemyIs Enemies.eternitysSentinel_016
          when isScourge do
            watcher <- getSetAsideCard Enemies.eternitysSentinel_017
            push $ ReplaceEnemy sentinel watcher Swap
          placement <- field EnemyPlacement sentinel
          when (placement == Global)
            $ push
            $ EnemySpawnAtLocationMatching Nothing (locationIs Locations.rivertown) sentinel

      advanceActDeck attrs
      pure a
    {- "For the remainder of the scenario, this enemy gets +1[per_investigator]
    health and loses hunter." -}
    FoundEncounterCard _ (isTarget attrs -> True) (toCard -> card) -> do
      selectForMaybeM (locationIs Locations.winchmoreHouse) \winchmoreHouse -> do
        eid <- createEnemy card winchmoreHouse
        n <- perPlayer 1
        gameModifiers attrs eid [HealthModifier n, RemoveKeyword Keyword.Hunter]
      pure a
    _ -> GettingYourBearings <$> liftRunMessage msg attrs

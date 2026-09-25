{- | Transdimensional Shock (The World Unbidden's B side) reads "Flip any number
of [[Stable]] keys they control to their [[Unstable]] side to cancel that much
damage/horror just dealt to them by the above effect." The implementation offered
exactly one flip cancelling exactly one point, so two of the four points the
agenda deals could never be cancelled (#5770).
-}
module Arkham.Agenda.Cards.TheScarletKeys.CongressOfTheKeys.TheWorldUnbiddenSpec (spec) where

import Arkham.Agenda.CardDefs.TheScarletKeys.CongressOfTheKeys qualified as Agendas
import Arkham.Campaigns.TheScarletKeys.Key.Cards qualified as Keys
import Arkham.Campaigns.TheScarletKeys.Key.Matcher
import Arkham.Entities qualified as Entities
import Arkham.Placement
import TestImport.New

-- | Put the agenda into play on its own, without the rest of the scenario.
realAgenda :: CardDef -> TestAppT AgendaId
realAgenda def = do
  card <- genCard def
  let aid = AgendaId (toCardCode card)
  overTest $ entitiesL . Entities.agendasL %~ insertEntity (lookupAgenda aid 2 (toCardId card))
  pure aid

-- | Attach a Scarlet Key to the investigator; keys borne by one enter play Stable.
putScarletKeyIntoPlay :: CardDef -> Investigator -> TestAppT ()
putScarletKeyIntoPlay def self = do
  card <- genCard def
  run $ CreateScarletKeyAt card (AttachedToInvestigator $ toId self)

fourKeys :: [CardDef]
fourKeys = [Keys.theLastBlossom, Keys.theWeepingLady, Keys.theEyeOfRavens, Keys.theMirroringBlade]

-- | The options are scenario-scoped i18n keys, so match on the suffix.
clicking :: HasCallStack => Text -> TestAppT ()
clicking key = chooseOptionMatching "choose an option" \case
  Label label _ -> key `isInfixOf` label
  _ -> False

{- | Drive the stable-key half of the ability directly. The real entry point is a
silent forced reaction to the agenda's own damage, which carries the amounts in
its window; the step below is what that reaction resolves to.
-}
flipStableKeys :: Investigator -> AgendaId -> Int -> Int -> TestAppT ()
flipStableKeys self aid dmg hrr =
  run $ DoStep dmg $ DoStep hrr $ UseCardAbility (toId self) (AgendaSource aid) 1 [] NoPayment

spec :: Spec
spec = describe "The World Unbidden" do
  it "lets the investigator flip a stable key for each point cancelled" . gameTest $ \self -> do
    aid <- realAgenda Agendas.theWorldUnbidden
    traverse_ (`putScarletKeyIntoPlay` self) fourKeys
    flipStableKeys self aid 2 2
    for_ ["theWorldUnbidden.damage", "theWorldUnbidden.damage", "theWorldUnbidden.horror"] \key -> do
      chooseFirstOption "flip a stable key"
      clicking key
    selectCount StableScarletKey `shouldReturn` 1

  it "stops offering a cancel once that half is exhausted" . gameTest $ \self -> do
    aid <- realAgenda Agendas.theWorldUnbidden
    traverse_ (`putScarletKeyIntoPlay` self) fourKeys
    flipStableKeys self aid 1 0
    chooseFirstOption "flip a stable key"
    clicking "theWorldUnbidden.damage"
    -- Nothing left to cancel, so the loop ends rather than flipping a second key.
    selectCount StableScarletKey `shouldReturn` 3

  it "can be declined" . gameTest $ \self -> do
    aid <- realAgenda Agendas.theWorldUnbidden
    traverse_ (`putScarletKeyIntoPlay` self) fourKeys
    flipStableKeys self aid 2 2
    clicking "skip"
    selectCount StableScarletKey `shouldReturn` 4

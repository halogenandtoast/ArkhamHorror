{-# LANGUAGE ImplicitParams #-}

{- | "The Innsmouth Conspiracy of Olaus Wormius" -- a community rewrite of the
campaign's prose by Zach Holbrook, writing as Olaus Wormius.

The rewrite changes nothing mechanically, so everything here is narrative. A
block it only rewords needs nothing in this module: the client resolves the key
the engine already emits against the variant's messages. What lives here is the
rest -- the blocks it splits by the Lead Investigator's identity or by the
campaign log, the ones it builds from several pieces, and the two it does not
tell at all.

The beats are gathered into 'narrative', the record the campaign reads them off.
The campaign's own code never names this rewrite -- only the registry does.
-}
module Arkham.Narrative.OlausWormius.TheInnsmouthConspiracy where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Campaigns.TheInnsmouthConspiracy.Helpers (Suspect (..))
import Arkham.Campaigns.TheInnsmouthConspiracy.Key
import Arkham.ClassSymbol
import Arkham.Classes.HasGame
import Arkham.Helpers.Campaign (getOwner)
import Arkham.Helpers.FlavorText
import Arkham.Helpers.Log
import Arkham.Helpers.Narrative
import Arkham.Helpers.Query
import Arkham.I18n
import Arkham.Investigator.Projection ()
import Arkham.Investigator.Types (Field (InvestigatorBaseAgility))
import Arkham.Message.Lifted.Queue (ReverseQueue)
import Arkham.Narrative.TheInnsmouthConspiracy.Types
import Arkham.Prelude
import Arkham.Projection
import Arkham.Trait

{- | The name this rewrite is selected and keyed under. Registered against it in
"Arkham.Narrative.TheInnsmouthConspiracy".
-}
variant :: Text
variant = "olausWormius"

{- | The beats this rewrite changes. Everything it only rewords resolves from its
own messages at the key the engine already emits, so it is absent here.
-}
narrative :: (HasI18n, ReverseQueue m) => InnsmouthNarrative m
narrative =
  noInnsmouthNarrative
    { thePitOfDespairFlashback1 = Just thePitOfDespairFlashback1
    , interlude1Part2 = Just interlude1Part2
    , theVanishingOfElinaHarperIntro1 = Just theVanishingOfElinaHarperIntro1
    , omitsTheVanishingOfElinaHarperIntro2 = True
    , inTooDeepFlashback6 = Just inTooDeepFlashback6
    , inTooDeepFlashback8 = Just inTooDeepFlashback8
    , interlude3NoKeys = Just interlude3NoKeys
    , interlude4HiddenTruths = Just interlude4HiddenTruths
    , interlude4RightfulKeeper = Just interlude4RightfulKeeper
    , intoTheMaelstromIntro = Just intoTheMaelstromIntro
    , omitsEpilogue2 = True
    }

-- * Scenario I: The Pit of Despair

{- | Flashback I opens four ways on the office in the Custom House tower -- and a
fifth time for Silas Marsh, who is from the town they are about to drive to. The
guide's consequence lines close the panel either way.
-}
thePitOfDespairFlashback1 :: (HasI18n, HasGame m) => m (FlavorTextBuilder ())
thePitOfDespairFlashback1 = scope "flashback1" do
  silas <- leadIsNamed ["Silas Marsh"]
  veteran <- leadHasTrait Veteran
  mystic <- leadIsClass Mystic
  agency <- leadHasTrait Agency
  pure do
    setTitle "title"
    blueBlock do
      h "title"
      p "intro"
      when silas $ p "silasMarsh"
      if
        | veteran -> p "veteran"
        | mystic -> p "mystic"
        | agency -> p "agency"
        | otherwise -> pure ()
      p "rest"
      p "tail"

-- * Interlude I: Puzzle Pieces

{- | The walk into Innsmouth: how quietly you manage it depends on how quick the
Lead Investigator is, and a scientist recognizes what is bolted to the skiff. The
rewrite's footnote on the lighthouse rides at the foot of @rest@, because a beat
it only rewords has no call site to put a 'footnote' in and the two must match.
-}
interlude1Part2 :: (HasI18n, HasGame m) => m (FlavorTextBuilder ())
interlude1Part2 = scope "part2" do
  lead <- getLead
  agility <- field InvestigatorBaseAgility lead
  science <- leadHasTrait Science
  pure do
    setTitle "title"
    p "intro"
    p $ if agility >= 4 then "agility" else "otherwise"
    p "flophouse"
    when science $ p "science"
    p "rest"

-- * Scenario II: The Vanishing of Elina Harper

-- | The first day in town, with Silas Marsh seeing it as he remembers it.
theVanishingOfElinaHarperIntro1 :: (HasI18n, HasGame m) => m (FlavorTextBuilder ())
theVanishingOfElinaHarperIntro1 = scope "intro1" do
  silas <- leadIsNamed ["Silas Marsh"]
  pure do
    setTitle "title"
    h "title"
    p "ignore"
    p "body"
    when silas $ p "silasMarsh"
    p "rest"

-- * Scenario III: In Too Deep

-- | Whether the Lead Investigator knows the psalm Dawson reads out.
inTooDeepFlashback6 :: (HasI18n, HasGame m) => m (FlavorTextBuilder ())
inTooDeepFlashback6 = scope "flashback6" do
  believer <- leadHasTrait Believer
  pure do
    setTitle "title"
    blueBlock do
      h "title"
      p "intro"
      p $ if believer then "believer" else "otherwise"
      p "rest"
      p "tail"

{- | Three mornings in the Innsmouth jail, with whoever took Agent Harper. The
kidnapper is the name circled under "Possible Suspects".
-}
inTooDeepFlashback8 :: (HasI18n, ReverseQueue m) => m (FlavorTextBuilder ())
inTooDeepFlashback8 = scope "flashback8" do
  suspect <- (maybeResult =<<) <$> getCircledRecord PossibleSuspects
  pure do
    setTitle "title"
    blueBlock do
      h "title"
      p "intro"
      for_ suspect \case
        BrianBurnham -> p "brianBurnham"
        OtheraGilman -> p "otheraGilman"
        JoyceLittle -> p "joyceLittle"
        BarnabasMarsh -> p "barnabasMarsh"
        ZadokAllen -> p "zadokAllen"
        RobertFriendly -> p "robertFriendly"
      p "rest"
      p "tail"

-- * Interlude III: Beneath the Waves

{- | What Harper is told when the investigators came away from Devil Reef with
nothing. The Campaign Guide has no block here, so this is narrative the rewrite
adds rather than one it replaces.
-}
interlude3NoKeys :: (HasI18n, ReverseQueue m) => m ()
interlude3NoKeys = flavor $ p "noKeys"

-- * Interlude IV: Hidden Truths

-- | Harper on piezoelectricity, to an audience that may already follow her.
interlude4HiddenTruths :: (HasI18n, HasGame m) => m (FlavorTextBuilder ())
interlude4HiddenTruths = scope "hiddenTruths" do
  science <- leadHasTrait Science
  pure do
    setTitle "title"
    p "intro"
    p $ if science then "science" else "otherwise"
    p "rest"

{- | The relic that answers the gateway is the one somebody kept, so the scene is
told about that relic rather than about relics in general.
-}
interlude4RightfulKeeper :: HasGame m => m Scope
interlude4RightfulKeeper = do
  idol <- isJust <$> getOwner Assets.wavewornIdol
  mantle <- isJust <$> getOwner Assets.awakenedMantle
  pure
    $ if
      | idol -> "wavewornIdol"
      | mantle -> "awakenedMantle"
      | otherwise -> "headdressOfYhaNthlei"

-- * Scenario VIII: Into the Maelstrom

-- | Harper's case against the water, with the mantle as her example of it.
intoTheMaelstromIntro :: (HasI18n, HasGame m) => m (FlavorTextBuilder ())
intoTheMaelstromIntro = scope "intro" do
  mantle <- isJust <$> getOwner Assets.awakenedMantle
  pure do
    setTitle "title"
    h "title"
    p "ignore"
    p "body"
    when mantle $ p "awakenedMantle"

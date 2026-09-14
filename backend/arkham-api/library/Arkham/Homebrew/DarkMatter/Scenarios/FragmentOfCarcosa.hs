module Arkham.Homebrew.DarkMatter.Scenarios.FragmentOfCarcosa (fragmentOfCarcosa) where

import Arkham.Card (CardDef)
import Arkham.Enemy.CardDefs.ThePathToCarcosa.InhabitantsOfCarcosa qualified as InhabitantsOfCarcosa
import Arkham.Helpers.Act (getCurrentActStepMaybe)
import Arkham.Helpers.FlavorText
import Arkham.Helpers.GameValue (getGameValue)
import Arkham.Homebrew.DarkMatter.CardDefs.Acts qualified as Acts
import Arkham.Homebrew.DarkMatter.CardDefs.Agendas qualified as Agendas
import Arkham.Homebrew.DarkMatter.CardDefs.Assets qualified as Assets
import Arkham.Homebrew.DarkMatter.CardDefs.Enemies qualified as Enemies
import Arkham.Homebrew.DarkMatter.CardDefs.Locations qualified as Locations
import Arkham.Homebrew.DarkMatter.CardDefs.Stories qualified as Stories
import Arkham.Homebrew.DarkMatter.Helpers (
  addImpendingDoom,
  crossOffMemories,
  earnXp,
  getMemories,
  scenarioI18n,
 )
import Arkham.Homebrew.DarkMatter.Key
import Arkham.Homebrew.DarkMatter.Sets qualified as Set
import Arkham.Homebrew.DarkMatter.Traits (pattern Carcosa)
import Arkham.Location.Types (Field (LocationClues, LocationRevealClues))
import Arkham.Matcher
import Arkham.Message (ReplaceStrategy (Swap))
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log
import Arkham.Projection
import Arkham.Resolution
import Arkham.Scenario.Import.Lifted
import Arkham.Trait (Trait (Cave))

newtype FragmentOfCarcosa = FragmentOfCarcosa ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fragmentOfCarcosa :: Difficulty -> FragmentOfCarcosa
fragmentOfCarcosa difficulty =
  scenario
    FragmentOfCarcosa
    ":dark-matter:209"
    "Fragment of Carcosa"
    difficulty
    [ "circle square ."
    , ". triangle ."
    , ". plus ."
    , "hourglass diamond trefoil"
    , ". t ."
    ]

{- | Scenario reference card, ":dark-matter:209" (the front block is
Easy \/ Standard, from @cards\/foc_01_fragment_of_carcosa.json@; the
Hard \/ Expert block is user-supplied in @act-backs.md@):

Easy \/ Standard
[skull]: -2. If you fail while you are at a [[Cave]] location, take 1 damage.
[cultist]: -2. If you fail while you are at a [[Carcosa]] location, take 1 horror.
[tablet]: -1. You must either (choose one): Reveal another token, or cross out 1 tally mark next to your "Memories".
[elder thing]: -2. If you are at a [[Carcosa]] location: Add 1 doom to the current agenda, or automatically fail this skill test.

Hard \/ Expert
[skull]: -3. If you are at a [[Cave]] location, take 1 damage.
[cultist]: -3. If you are at a [[Carcosa]] location, take 1 horror.
[tablet]: -2. You must either (choose one): Reveal another token, or cross out 1 tally mark next to your "Memories".
[elder thing]: -3. If you are at a [[Carcosa]] location: Add 1 doom to the current agenda, or automatically fail this skill test.

Only the /values/ live here; the riders are the 'ResolveChaosToken' and
'FailedSkillTest' cases in 'RunMessage' below. [skull] and [cultist] lose their
"if you fail" condition on hard\/expert, so those two fire on reveal instead.
-}
instance HasChaosTokenValue FragmentOfCarcosa where
  getChaosTokenValue iid tokenFace (FragmentOfCarcosa attrs) = case tokenFace of
    Skull -> pure $ toChaosTokenValue attrs Skull 2 3
    Cultist -> pure $ toChaosTokenValue attrs Cultist 2 3
    Tablet -> pure $ toChaosTokenValue attrs Tablet 1 2
    ElderThing -> pure $ toChaosTokenValue attrs ElderThing 2 3
    otherFace -> getChaosTokenValue iid otherFace attrs

{- | "Set aside the 6 double-sided locations, out of play." Each is set aside on
its [[Cave]] face; the [[Carcosa]] face is the 'cdOtherSide' it flips to, so act
1b putting the set-aside card into play is what "Cave side face up" means.
-}
doubleSidedLocations :: [CardDef]
doubleSidedLocations =
  [ Locations.bottomlessPit
  , Locations.cyclopeanCaverns
  , Locations.hiddenPassage
  , Locations.iceCavity
  , Locations.impassableRavine
  , Locations.stalagmiteForest
  ]

{- | "Set aside the 4 one-sided story cards (Delights, Arrival of the King, For
You Alone and Lost Expedition), out of play."
-}
storyCards :: [CardDef]
storyCards =
  [ Stories.delights
  , Stories.arrivalOfTheKing
  , Stories.forYouAlone
  , Stories.lostExpedition
  ]

instance RunMessage FragmentOfCarcosa where
  runMessage msg s@(FragmentOfCarcosa attrs) = runQueueT $ scenarioI18n "fragmentOfCarcosa" $ case msg of
    PreScenarioSetup -> do
      flavor $ scope "intro" $ h "title" >> p "body"
      pure s
    Setup -> runScenarioSetup FragmentOfCarcosa attrs do
      setup $ ul do
        li "gatherSets"
        li.nested "placeLocations" do
          li "startAt"
        li "setAsideLocations"
        li "setAsideStoryCards"
        li "setAsideCards"
        unscoped $ li "shuffleRemainder"
        unscoped $ li "readyToBegin"

      -- "Gather all cards from the following encounter sets: Fragment of Carcosa
      -- and Inhabitants of Carcosa (from The Path to Carcosa)."
      gather Set.FragmentOfCarcosa
      gather Set.InhabitantsOfCarcosa

      setAgendaDeck [Agendas.theShadowsLengthen, Agendas.shallDryAndDie]
      setActDeck [Acts.inLostCarcosa, Acts.theHeirToCarcosa, Acts.theUnspeakableTruth]

      -- "Put the Surface of Fragment and Abandoned Lander locations into play.
      -- Each investigator begins play at Surface of Fragment."
      place_ Locations.abandonedLander
      startAt =<< place Locations.surfaceOfFragment

      setAside doubleSidedLocations
      setAside storyCards

      {- "Set aside the Bottle of Whispers story asset, the Cave Dweller enemy,
      the Beast of Aldebaran enemy, and each copy of Spawn of Hali aside, out of
      play." Inhabitants of Carcosa holds nothing else, so nothing from that set
      reaches the encounter deck. -}
      setAside [Assets.bottleOfWhispers, Enemies.caveDweller, InhabitantsOfCarcosa.beastOfAldebaran]
      setAsideEvery (cardIs InhabitantsOfCarcosa.spawnOfHali)
    {- [skull] \/ [cultist] on hard\/expert: "If you are at a [[Cave]] location,
    take 1 damage." \/ "If you are at a [[Carcosa]] location, take 1 horror." The
    easy\/standard sides only fire on a failed test, and are handled below. -}
    ResolveChaosToken _ Skull iid | isHardExpert attrs -> do
      whenM (iid <=~> InvestigatorAt (LocationWithTrait Cave)) $ assignDamage iid Skull 1
      pure s
    ResolveChaosToken _ Cultist iid | isHardExpert attrs -> do
      whenM (iid <=~> InvestigatorAt (LocationWithTrait Carcosa)) $ assignHorror iid Cultist 1
      pure s
    {- [tablet]: "You must either (choose one): Reveal another token, or cross out
    1 tally mark next to your 'Memories'." Mandatory, so with no tally marks left
    the only thing left to do is reveal another token. -}
    ResolveChaosToken _ Tablet iid -> do
      memories <- getMemories iid
      chooseOneM iid do
        labeled "tablet.revealAnotherToken" $ drawAnotherChaosToken iid
        when (memories > 0)
          $ labeled "tablet.crossOffMemory"
          $ crossOffMemories iid 1
      pure s
    {- [elder thing]: "If you are at a [[Carcosa]] location: Add 1 doom to the
    current agenda, or automatically fail this skill test." -}
    ResolveChaosToken _ ElderThing iid -> do
      whenM (iid <=~> InvestigatorAt (LocationWithTrait Carcosa)) do
        chooseOneM iid $ unscoped do
          countVar 1 $ labeled "placeDoomOnAgenda" $ placeDoomOnAgenda 1
          labeled "automaticallyFailTest" failSkillTest
      pure s
    {- [skull] \/ [cultist] on easy\/standard: "If you fail while you are at a
    [[Cave]] \/ [[Carcosa]] location, take 1 damage \/ horror." -}
    FailedSkillTest iid _ _ (ChaosTokenTarget token) _ _ | isEasyStandard attrs -> do
      case token.face of
        Skull -> whenM (iid <=~> InvestigatorAt (LocationWithTrait Cave)) $ assignDamage iid Skull 1
        Cultist ->
          whenM (iid <=~> InvestigatorAt (LocationWithTrait Carcosa)) $ assignHorror iid Cultist 1
        _ -> pure ()
      pure s
    {- Guide, "Flipping Locations in Fragment": "simply flip the location, keeping
    all tokens, attachments, investigators, enemies, and other cards on that same
    location (which will have a different name). Then, add clues on that location
    up to its clue value." 'Arkham.Homebrew.DarkMatter.Helpers.flipToOtherSide'
    defers this step until after the swap has resolved. -}
    DoStep 1 (ReplaceLocation lid _ Swap) -> do
      value <- getGameValue =<< field LocationRevealClues lid
      current <- field LocationClues lid
      when (value > current) $ placeClues ScenarioSource lid (value - current)
      pure s
    ScenarioResolution r -> scope "resolutions" do
      case r of
        {- "If each investigator was defeated and it is act 1 or act 2: Proceed to
        Resolution 1. If each investigator was defeated and it is act 3: Proceed
        to Resolution 2." -}
        NoResolution -> do
          actStep <- getCurrentActStepMaybe
          let onAct3 = actStep == Just 3
          resolutionFlavor do
            setTitle "noResolution.title"
            p "noResolution.body"
            ul do
              li.validate (not onAct3) "noResolution.proceedToResolution1"
              li.validate onAct3 "noResolution.proceedToResolution2"
          push $ if onAct3 then R2 else R1
        Resolution 1 -> do
          resolution "resolution1"
          record TheInvestigatorsAreTrappedWithinCarcosa
          eachInvestigator drivenInsane
          gameOver
        Resolution 2 -> do
          earnXp attrs "resolution2"
          addImpendingDoom 2
          endOfScenario
        Resolution 3 -> do
          earnXp attrs "resolution3"
          endOfScenario
        _ -> error "invalid resolution"
      pure s
    _ -> FragmentOfCarcosa <$> liftRunMessage msg attrs

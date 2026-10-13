module Arkham.Homebrew.AgainstTheWendigo.Assets.LostChild (lostChild) where

import Arkham.Ability
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelfWhen)
import Arkham.Helpers.SkillTest.Lifted (parley)
import Arkham.Homebrew.AgainstTheWendigo.CardDefs.Assets qualified as Cards
import Arkham.Homebrew.AgainstTheWendigo.Traits (pattern Civilized, pattern Sarcee)
import Arkham.Matcher hiding (AssetDefeated)
import Arkham.Message.Lifted.Placement

newtype LostChild = LostChild AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lostChild :: AssetCard LostChild
lostChild = allyWith LostChild Cards.lostChild (1, 1) noSlots

instance HasModifiersFor LostChild where
  -- Losing her takes her out of the game, the way the box's other Sarcee allies
  -- go. Only while she is on the table: the Revelation below is a real discard,
  -- surge and all.
  getModifiersFor (LostChild a) =
    modifySelfWhen a a.placement.isInPlay [RemoveFromGameInsteadOfDiscard]

instance HasAbilities LostChild where
  getAbilities (LostChild a) =
    {- "If an agenda or an act advances, or if Lost Child is removed from the
    game, each investigator takes 1 direct horror."

    Her card is still an entity once she is safe in the victory display, so the
    Forced has to be gone by then or it keeps charging for later advances.
    -}
    [ mkAbility a 1
        $ forced
        $ oneOf
          [ AgendaAdvances #when AnyAgenda
          , ActAdvances #when AnyAct
          , AssetWouldLeavePlay #when (be a)
          ]
    | a.placement.isInPlay
    ]
      <> [ -- "{action}: Parley. Take control of Lost Child."
           restricted a 2 (OnSameLocation <> not_ ControlsThis) parleyAction_
         , {- "{reaction} If you are in a Sarcee location: Put Lost Child in the
           victory display."

           An anytime reaction, like the Temple of Ithaqua reads: the condition is
           where you are standing, not a moment you arrive there, so walking her
           home has to offer it the instant you get there. Non-blocking so that it
           rides along with asks the window already stops for instead of
           interrupting every placed token and leave-play window on the way home. -}
           nonBlocking
             $ controlled a 3 (exists $ YourLocation <> LocationWithTrait Sarcee)
             $ freeReaction AnyWindow
         ]

instance RunMessage LostChild where
  runMessage msg a@(LostChild attrs) = runQueueT $ case msg of
    -- "If you are on a Civilized location, Lost Child gains Surge then discard
    -- her. Otherwise, attach this card to your location."
    Revelation iid (isSource attrs -> True) -> do
      civilized <- selectAny $ locationWithInvestigator iid <> LocationWithTrait Civilized
      if civilized
        then do
          push $ GainSurge (toSource attrs) (toTarget attrs)
          toDiscard attrs attrs
        else withLocationOf iid \lid -> place attrs (AttachedToLocation lid)
      pure a
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      eachInvestigator \iid -> directHorror iid (attrs.ability 1) 1
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      sid <- getRandom
      parley sid iid (attrs.ability 2) attrs #intellect (Fixed 2)
      pure a
    PassedThisSkillTest iid (isAbilitySource attrs 2 -> True) -> do
      takeControlOfAsset iid attrs.id
      pure a
    UseThisAbility iid (isSource attrs -> True) 3 -> do
      addToVictory iid attrs
      pure a
    {- Her "Victory 1" is the reward for bringing her to safety, so a defeat must
    not hand it over, which the attrs runner does for any asset worth points.

    A discard and not @removeFromGame@: the leave-play window the Forced above
    watches is raised by @RemoveFromPlay@, and a discard queues that /before/ it
    turns into the removal, while @RemoveFromGame@ deletes the entity outright
    and so raises nothing. @RemoveFromGameInsteadOfDiscard@ still lands her out
    of the game.
    -}
    Do (AssetDefeated source aid) | aid == attrs.id -> do
      toDiscard source attrs
      pure a
    _ -> LostChild <$> liftRunMessage msg attrs

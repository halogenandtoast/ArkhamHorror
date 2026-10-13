module Arkham.Homebrew.AgainstTheWendigo.Treacheries.Skinwalker (skinwalker) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ModifierType (..), modified_)
import Arkham.History.Types
import Arkham.Homebrew.AgainstTheWendigo.CardDefs.Treacheries qualified as Cards
import Arkham.Homebrew.AgainstTheWendigo.Traits (pattern Animal)
import Arkham.Keyword qualified as Keyword
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Placement
import Arkham.Treachery.Import.Lifted

newtype Skinwalker = Skinwalker TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

skinwalker :: TreacheryCard Skinwalker
skinwalker = treachery Skinwalker Cards.skinwalker

instance HasModifiersFor Skinwalker where
  getModifiersFor (Skinwalker a) =
    for_ a.attached.enemy \eid ->
      modified_
        a
        eid
        [ Blank
        , HorrorDealt 1
        , AddKeyword Keyword.Hunter
        , AddKeyword Keyword.Retaliate
        ]

instance HasAbilities Skinwalker where
  getAbilities (Skinwalker a) =
    [ restricted a 1 (maybe Never attackedThisRound a.attached.enemy)
        $ forced
        $ PhaseEnds #when #enemy
    ]
   where
    attackedThisRound eid = HasHistory RoundHistory Anyone (AttackedByEnemy eid)

instance RunMessage Skinwalker where
  runMessage msg t@(Skinwalker attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      candidates <- select $ NearestEnemyTo iid (EnemyWithTrait Animal)
      if null candidates
        then gainSurge attrs
        else chooseOrRunOneM iid $ targets candidates $ attachTreachery attrs
      pure t
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      place attrs Unplaced
      for_ attrs.attached.enemy $ shuffleBackIntoEncounterDeck (attrs.ability 1)
      shuffleBackIntoEncounterDeck (attrs.ability 1) attrs
      pure t
    _ -> Skinwalker <$> liftRunMessage msg attrs

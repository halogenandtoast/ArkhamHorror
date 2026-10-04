module Arkham.Homebrew.AgainstTheWendigo.Treacheries.Skinwalker (skinwalker) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ModifierType (..), modified_)
import Arkham.Homebrew.AgainstTheWendigo.CardDefs.Treacheries qualified as Cards
import Arkham.Homebrew.AgainstTheWendigo.Traits (pattern Animal)
import Arkham.Keyword qualified as Keyword
import Arkham.Matcher hiding (EnemyAttacks)
import Arkham.Matcher qualified as Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Treachery.Import.Lifted

newtype Skinwalker = Skinwalker TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

skinwalker :: TreacheryCard Skinwalker
skinwalker = treachery Skinwalker Cards.skinwalker

instance HasModifiersFor Skinwalker where
  {- | "The attached enemy loses all the printed text box (except for Traits)
  and gains +1 horror, Hunter and Retaliate." -}
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
    [ mkAbility a 1 $ forced $ PhaseEnds #when #enemy
    , -- There is no "attacked this round" enemy matcher, so the card remembers
      -- it for itself and forgets again when the round ends.
      mkAbility a 2 $ forced $ Matcher.EnemyAttacks #after Anyone AnyEnemyAttack (attachedEnemy a)
    ]

-- | The attached enemy, or nothing while Skinwalker is unattached.
attachedEnemy :: TreacheryAttrs -> EnemyMatcher
attachedEnemy a = maybe (NotEnemy AnyEnemy) EnemyWithId a.attached.enemy

instance RunMessage Skinwalker where
  runMessage msg t@(Skinwalker attrs) = runQueueT $ case msg of
    -- "If there is no Animal enemy in play, Skinwalker gains Surge, then discard
    -- him. Otherwise, attach Skinwalker to the nearest Animal enemy."
    Revelation iid (isSource attrs -> True) -> do
      candidates <- select $ NearestEnemyTo iid (EnemyWithTrait Animal)
      if null candidates
        then do
          gainSurge attrs
          toDiscard attrs attrs
        else chooseOrRunOneM iid $ targets candidates $ attachTreachery attrs
      pure t
    -- "At the end of the enemy phase, if the attached enemy has attacked this
    -- round: Shuffle the attached enemy and Skinwalker into the encounter deck."
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      for_ attrs.attached.enemy \eid ->
        when (toResultDefault False attrs.meta) do
          push $ ShuffleBackIntoEncounterDeck (toSource attrs) (toTarget eid)
          push $ ShuffleBackIntoEncounterDeck (toSource attrs) (toTarget attrs)
      pure $ Skinwalker $ setMeta False attrs
    UseThisAbility _ (isSource attrs -> True) 2 ->
      pure $ Skinwalker $ setMeta True attrs
    _ -> Skinwalker <$> liftRunMessage msg attrs

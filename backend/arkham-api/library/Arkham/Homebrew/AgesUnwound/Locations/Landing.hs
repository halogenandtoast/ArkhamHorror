module Arkham.Homebrew.AgesUnwound.Locations.Landing (landing) where

import Arkham.Ability
import Arkham.Criteria qualified as Criteria
import Arkham.Effect.Window (EffectWindow (EffectActionWindow))
import Arkham.GameValue
import Arkham.Helpers.Modifiers (modifyEach)
import Arkham.Helpers.Query (getLead)
import Arkham.Homebrew.AgesUnwound.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.AgesUnwound.Scenarios.TheMyriadGentleman.Helpers
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Modifier
import Arkham.Trait (Trait (Firearm, Ranged, Spell))

newtype Landing = Landing LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

landing :: LocationCard Landing
landing = symbolLabel $ location Landing Cards.landing 2 (PerPlayer 1)

{- Ability 2 reacts to *activating* a Fight ability, so the ability has to be
offered in the first place. 'canDoAction' gates a fight on
'EnemyFightActionCriteria' read from @AbilityTarget iid ab.ref@, which is why
Marksmanship (1) and Telescopic Sight (3) both publish the widened criteria
from the card and not only from their effect. Without this, Shrivelling's
fight never appears at Landing while the only enemy is in the Entrance Hall,
and the reaction can never fire. 'CanModify' keeps it out of target selection
('ChooseFightEnemy' runs under 'withoutCanModifiers'), where the effect's own
override takes over. -}
instance HasModifiersFor Landing where
  getModifiersFor (Landing a) = whenRevealed a do
    inEntranceHall <- selectAny $ EnemyAt (locationIs Cards.entranceHall)
    when inEntranceHall do
      iids <- select (investigatorAt a)
      for_ iids \iid -> do
        abilities <-
          select
            $ AbilityIsAction #fight
            <> AbilityOnCardControlledBy iid
            <> AbilityOnCard (oneOf [CardWithTrait t | t <- [Ranged, Firearm, Spell]])
        modifyEach
          a
          (map (AbilityTarget iid . abilityToRef) abilities)
          [ CanModify
              $ EnemyFightActionCriteria
              $ CriteriaOverride
              $ Criteria.EnemyCriteria
              $ Criteria.ThisEnemy
              $ EnemyWithoutModifier CannotBeAttacked
              <> EnemyAt (oneOf [locationIs Cards.entranceHall, be a])
          ]

instance HasAbilities Landing where
  getAbilities (Landing a) =
    extendRevealed
      a
      [ {- "Forced - After you reveal Landing, if there are at least 3 players in
        the game: Spawn 2 copies of The Myriad Gentleman at Landing." -}
        restricted a 1 (Criteria.AnyCriterion [Criteria.PlayerCountIs 3, Criteria.PlayerCountIs 4])
          $ forced
          $ RevealLocation #after You (be a)
      , {- "[reaction] When you activate a Fight ability on a [[Ranged, Firearm]]
        or [[Spell]] card: This attack targets an enemy in the Entrance Hall.
        Ignore the aloof and retaliate keywords for this attack." -}
        restricted a 2 (Here <> exists (EnemyAt $ locationIs Cards.entranceHall))
          $ freeReaction
          $ ActivateAbility #when You
          $ AbilityIsAction #fight
          <> AbilityOnCard (oneOf [CardWithTrait t | t <- [Ranged, Firearm, Spell]])
      ]

instance RunMessage Landing where
  runMessage msg l@(Landing attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      lead <- getLead
      spawnMyriadCopiesAt lead 2 attrs.id
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      {- The override rides on the investigator, which is where
      'ChooseFightEnemy' reads 'EnemyFightActionCriteria' from (the Telescopic
      Sight (3) effect does the same). It is bare rather than 'CanModify' so it
      survives the 'withoutCanModifiers' that selection runs under. "Targets an
      enemy in the Entrance Hall" is mandatory, so the override REPLACES the
      normal same-location criterion rather than widening it. -}
      createWindowModifierEffect_
        EffectActionWindow
        (attrs.ability 2)
        iid
        [ EnemyFightActionCriteria
            $ CriteriaOverride
            $ Criteria.EnemyCriteria
            $ Criteria.ThisEnemy
            $ EnemyWithoutModifier CannotBeAttacked
            <> EnemyAt (locationIs Cards.entranceHall)
        , IgnoreAloof
        , IgnoreRetaliate
        ]
      pure l
    _ -> Landing <$> liftRunMessage msg attrs

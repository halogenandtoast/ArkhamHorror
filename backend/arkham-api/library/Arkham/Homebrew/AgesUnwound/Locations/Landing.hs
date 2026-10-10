module Arkham.Homebrew.AgesUnwound.Locations.Landing (landing) where

import Arkham.Ability
import Arkham.Criteria qualified as Criteria
import Arkham.Effect.Window (EffectWindow (EffectActionWindow))
import Arkham.GameValue
import Arkham.Helpers.Query (getLead)
import Arkham.Homebrew.AgesUnwound.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.AgesUnwound.Scenarios.TheMyriadGentleman.Helpers
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Modifier
import Arkham.Trait (Trait (Firearm, Ranged, Spell))

newtype Landing = Landing LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

landing :: LocationCard Landing
landing = symbolLabel $ location Landing Cards.landing 2 (PerPlayer 1)

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
      'Game.CanFightEnemy' reads 'EnemyFightActionCriteria' from (the
      Telescopic Sight (3) effect does the same). "Targets an enemy in the
      Entrance Hall" is mandatory, so the override REPLACES the normal
      same-location criterion rather than widening it. -}
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

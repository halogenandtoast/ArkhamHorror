module Arkham.Event.Cards.DelayTheInevitable (
  delayTheInevitable,
  DelayTheInevitable (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Placement
import Arkham.Projection
import Arkham.Window (Window (..))
import Arkham.Window qualified as Window

newtype DelayTheInevitable = DelayTheInevitable EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

delayTheInevitable :: EventCard DelayTheInevitable
delayTheInevitable = event DelayTheInevitable Cards.delayTheInevitable

instance HasAbilities DelayTheInevitable where
  getAbilities (DelayTheInevitable a) =
    [ restrictedAbility a 1 ControlsThis $ ReactionAbility (DealtDamageOrHorror #when AnySource You) Free
    , restrictedAbility a 2 ControlsThis $ ForcedAbility $ PhaseEnds #when #mythos
    ]

getDamageAndHorror :: [Window] -> (Int, Int)
getDamageAndHorror [] = error "wrong window"
getDamageAndHorror ((windowType -> Window.WouldTakeDamageOrHorror _ _ damage horror) : _) =
  (damage, horror)
getDamageAndHorror (_ : xs) = getDamageAndHorror xs

instance RunMessage DelayTheInevitable where
  runMessage msg e@(DelayTheInevitable attrs) = case msg of
    PlayThisEvent iid eid | eid == toId attrs -> do
      iids <- selectList $ affectsOthers $ colocatedWith iid
      player <- getPlayer iid
      push
        $ chooseOrRunOne
          player
          [ targetLabel iid [PlaceEvent iid eid $ InPlayArea investigator]
          | investigator <- iids
          ]
      pure e
    UseCardAbility iid (isSource attrs -> True) 1 (getDamageAndHorror -> (damage, horror)) _ -> do
      player <- getPlayer iid
      pushAll
        [ toDiscardBy iid (toAbilitySource attrs 1) attrs
        , chooseOrRunOne player
            $ [Label "Cancel Damage" [CancelDamage iid damage] | damage > 0]
            <> [Label "Cancel Horror" [CancelHorror iid horror] | horror > 0]
            <> [ Label
                "Cancel Horror and Damage"
                [CancelDamage iid damage, CancelHorror iid horror]
               | damage > 0 && horror > 0
               ]
        ]
      pure e
    UseCardAbility iid (isSource attrs -> True) 2 _ _ -> do
      canAfford <- fieldMap InvestigatorResources (> 2) iid
      player <- getPlayer iid
      push
        $ chooseOrRunOne player
        $ [Label "Spend 2 Resources" [SpendResources iid 2] | canAfford]
        <> [ Label
              "Discard Delay the Invevitabe"
              [toDiscardBy iid (toAbilitySource attrs 2) attrs]
           ]
      pure e
    _ -> DelayTheInevitable <$> runMessage msg attrs

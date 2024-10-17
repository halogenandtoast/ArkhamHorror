<script lang="ts" setup>
import { watch, onMounted, onBeforeUnmount, computed, ref } from 'vue';
import { type Game } from '@/arkham/types/Game';

export interface Props {
  game: Game
  playerId: string
}

const props = defineProps<Props>()

const locations = computed(() => Object.values(props.game.locations).
  filter((a) => a.inFrontOf === null && a.label !== "cosmos"))

const toConnection = (div1: HTMLElement, div2: HTMLElement): string | undefined => {
  const [leftDiv, rightDiv] = [div1, div2].sort((a, b) => {
    const { id: div1Id } = a.dataset
    const { id: div2Id } = b.dataset

    if(!div1Id || !div2Id) {
      return 0
    }

    if (div1Id < div2Id) {
      return -1;
    }
    if (div1Id > div2Id) {
      return 1;
    }

    // names must be equal
    return 0;
  })

  const { id: leftDivId } = leftDiv.dataset
  const { id: rightDivId } = rightDiv.dataset

  if (leftDivId && rightDivId) {
    return leftDivId + ":" + rightDivId
  }
}

const makeLine = function(div1: HTMLElement, div2: HTMLElement) {
  const [leftDiv, rightDiv] = [div1, div2].sort((a, b) => {
    const { id: div1Id } = a.dataset
    const { id: div2Id } = b.dataset

    if (!div1Id || !div2Id) return 0
    if (div1Id < div2Id) return -1
    if (div1Id > div2Id) return 1

    // names must be equal
    return 0;
  })

  const closeEnough = (a: string, b: number) => {
    return diff(a, b) < 0.5
  }

  const diff = (a: string, b: number) => {
    const x = parseFloat(a)
    const y = b

    return Math.abs(x - y)
  }

  const { id: leftDivId } = leftDiv.dataset
  const { id: rightDivId } = rightDiv.dataset

  if (!leftDivId || !rightDivId) return

  const thisEl = document.getElementById("svg")
  if(!thisEl) return
  const line = document.querySelector<HTMLElement>(".line")
  const parentNode = line?.parentNode

  if (!line || !parentNode) return

  const connection = leftDivId + ":" + rightDivId
  const {left: bodyLeft, top: bodyTop} = thisEl.getBoundingClientRect()
  const {left: leftDivLeft, top: leftDivTop, right: leftDivRight, bottom: leftDivBottom } = leftDiv.getBoundingClientRect();
  const {left: rightDivLeft, top: rightDivTop, right: rightDivRight, bottom: rightDivBottom } = rightDiv.getBoundingClientRect();
  const leftDivWidth = leftDivRight - leftDivLeft;
  const rightDivWidth = rightDivRight - rightDivLeft;
  const leftDivHeight = leftDivTop - leftDivBottom;
  const rightDivHeight = rightDivTop - rightDivBottom;
  const x1 = (leftDivLeft - bodyLeft) + (leftDivWidth/2)
  const y1 = (leftDivTop - bodyTop) - (leftDivHeight/2)
  const x2 = (rightDivLeft - bodyLeft) + (rightDivWidth/2)
  const y2 = (rightDivTop - bodyTop) - (rightDivHeight/2)
  const existingNode = document.querySelector(`[data-connection="${connection}"]`)


  const isNodeClose = (node: Element) => {
    const ex1 = node.getAttribute("x1") || "-1"
    const ey1 = node.getAttribute("y1") || "-1"
    const ex2 = node.getAttribute("x2") || "-1"
    const ey2 = node.getAttribute("y2") || "-1"

    return closeEnough(ex1, x1) && closeEnough(ey1, y1) && closeEnough(ex2, x2) && closeEnough(ey2, y2)
  }

  const close = existingNode ? isNodeClose(existingNode) : false
  const investigator = Object.values(props.game.investigators).find(i => i.playerId == props.playerId)
  if (!investigator) return
  const { connectedLocations } = investigator
  const atLeft = leftDivId == investigator.location && connectedLocations.includes(rightDivId)
  const atRight = rightDivId == investigator.location && connectedLocations.includes(leftDivId)
  const activeLine = atLeft || atRight

  if (close) {
    if (activeLine) {
      existingNode?.classList.add("active")
    }
  } else {
    if (existingNode && !close) {
      const ex1 = existingNode.getAttribute("x1") || "-1"
      const ey1 = existingNode.getAttribute("y1") || "-1"
      const ex2 = existingNode.getAttribute("x2") || "-1"
      const ey2 = existingNode.getAttribute("y2") || "-1"

      if (closeEnough(ex1, x1) && closeEnough(ey1, y1) && closeEnough(ex2, x2) && closeEnough(ey2, y2)) {
        return
      }

      parentNode.removeChild(existingNode);
    }

    const node = line.cloneNode(true) as HTMLElement
    node.dataset.connection = connection
    node.classList.remove("original")
    node.classList.add("connection")

    if (activeLine) node.classList.add("active")

    parentNode.insertBefore(node, line.nextSibling)

    node.setAttribute('x1',x1.toString())
    node.setAttribute('y1',y1.toString())
    node.setAttribute('x2',x2.toString())
    node.setAttribute('y2',y2.toString())
  }
}

function handleConnections() {
  let activeConnections = [] as string[]
  for(const location of locations.value) {
    const { id, connectedLocations } = location
    const connections = typeof connectedLocations == "object"
      ? Object.values(connectedLocations)
      : connectedLocations

    connections.forEach((connection) => {
      const start = document.querySelector(`[data-id="${id}"]`) as HTMLElement
      const end = document.querySelector(`[data-id="${connection}"]`) as HTMLElement
      if (!start || !end) return

      const conn = toConnection(start, end)
      if (!conn) return
      if (location.modifiers.some((m) => m.type.tag == "DoNotDrawConnection" && conn == `${m.type.contents[0]}:${m.type.contents[1]}`)) return

      activeConnections.push(conn)
      makeLine(start, end)
    })
  }

  const rendered = document.querySelectorAll(".connection")

  for(const node of rendered) {
    const connection = (node as HTMLElement).dataset.connection
    if (!connection) node.parentNode?.removeChild(node)

    if (connection && !activeConnections.includes(connection)) {
      node.parentNode?.removeChild(node)
    }
  }

}

const requestId = ref<number | undefined>(undefined)
const drawHandler = () => {
  requestId.value = undefined
  handleConnections()
  requestId.value = window.requestAnimationFrame(drawHandler);
}

onBeforeUnmount(() => { if (requestId.value) window.cancelAnimationFrame(requestId.value) })
onMounted(() => requestId.value = window.requestAnimationFrame(drawHandler))

watch(locations, () => drawHandler())
</script>

<template>
  <svg id="svg">
    <line id="line" class="line original" stroke-dasharray="5, 5"/>
  </svg>
</template>

<style lang="scss" scoped>

#svg {
  pointer-events: none;
  position: absolute;
  isolation: isolate;
  top: 0;
  left: 0;
  width: 100%;
  height: 100%;
  z-index: -100000000;
}

#line{
  stroke-width:6px;
  /* stroke:#a6b5bb; */
  stroke:rgba(0,0,0, 0.2);

  @media (prefers-color-scheme: dark) {
    stroke:rgba(255,255,255, 0.2);
  }
}

.active {
  stroke:rgba(0,0,0,0.5) !important;

  @media (prefers-color-scheme: dark) {
    stroke:rgba(255,255,255, 0.7) !important;
  }
}
</style>
